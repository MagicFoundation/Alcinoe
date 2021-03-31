/// MongoDB document-oriented database direct access classes
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynMongoDB;

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

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - BBackSoon
  - Sabbiolina
  - Zed

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

  TODO:
  - handle BULK commands support for MongoDB >=2.6 for faster writes
    see http://blog.mongodb.org/post/84922794768
  - GridFS support ?

}

interface

{$I Synopse.inc} // define HASINLINE CPU32 CPU64

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$else}
  {$ifdef KYLIX3}
  SynKylix,
  LibC,
  {$else}
  SynFPCLinux,
  {$endif}
  {$endif}
  Classes,
  Variants,  // this unit expects Variants to be available for storage
  SysUtils,
  SynCrtSock,
  SynCrypto, // MD5 and SHA1 needed for OpenAuth()
  SynCommons,
  SynTable, // for TSynTableStatement
  SynLog;


{ ************ BSON (Binary JSON) process }

type
  /// binary representation of a 128-bit decimal, stored as 16 bytes
  // - i.e. IEEE 754-2008 128-bit decimal floating point as used in the
  // BSON Decimal128 format, and processed by the TDecimal128 object
  TDecimal128Bits = record
    case integer of
    0: (lo, hi: QWord);
    1: (l, h: Int64);
    2: (b: array[0..15] of byte);
    3: (c: array[0..3] of cardinal);
  end;
  /// points to a 128-bit decimal binary
  PDecimal128Bits = ^TDecimal128Bits;

  /// enough characters to contain any TDecimal128 text representation
  TDecimal128Str = array[0..42] of AnsiChar;

  /// some special 128-bit decimal values
  // - see TDecimal128.SetSpecial to set the corresponding value
  // - dsvError is returned by TDecimal128.FromText() on parsing error
  // - dsvValue indicates that this is not a known "special" value, but some
  // valid decimal number
  TDecimal128SpecialValue = (
    dsvError, dsvValue, dsvNan, dsvZero, dsvPosInf, dsvNegInf, dsvMin, dsvMax);

  /// handles a 128-bit decimal value
  // - i.e. IEEE 754-2008 128-bit decimal floating point as used in the
  // BSON Decimal128 format, i.e. betDecimal128 TBSONElementType
  // - the betFloat BSON format stores a 64-bit floating point value, which
  // doesn't have exact decimals, so may suffer from rounding or approximation
  // - for instance, if you work with Delphi currency values, you may store
  // betDecimal128 values in MongoDB - the easiest way is to include it as a
  // TBSONVariant instance, via the NumberDecimal() function
  // - there is no mathematical operator/methods for Decimal128 Value Objects,
  // as required by MongoDB specifications: any computation must be done
  // explicitly on native language value representation (e.g. currency, TBCD or
  // any BigNumber library) - use ToCurr/FromCurr or ToText/FromText to make
  // the appropriate safe conversions
  {$ifdef USERECORDWITHMETHODS}TDecimal128 = record
    {$else}TDecimal128 = object{$endif}
  public
    /// the raw binary storage
    Bits: TDecimal128Bits;
    /// fills with the Zero value
    // - note: under IEEE 754, Zero can have sign and exponents, so is not Hi=Lo=0
    // - is the same as Fill(dsvZero)
    procedure SetZero;
    /// fills with a special value
    // - dsvError or dsvValue will set dsvNan binary content
    procedure SetSpecial(special: TDecimal128SpecialValue);
    /// checks if the value matches one of the known special values
    // - will search for dsvNan, dsvZero, dsvPosInf, dsvNegInf, dsvMin, dsvMax
    function IsSpecial: TDecimal128SpecialValue;
    /// fills with a 32-bit signed value
    procedure FromInt32(value: integer);
    /// fills with a 32-bit unsigned value
    procedure FromUInt32(value: cardinal);
      {$ifdef HASINLINE}inline;{$endif}
    /// fills with a 64-bit signed value
    procedure FromInt64(value: Int64);
    /// fills with a 64-bit unsigned value
    procedure FromQWord(value: QWord);
      {$ifdef HASINLINE}inline;{$endif}
    /// fills with a fixed decimal value, as stored in currency
    // - will store the content with explictly four decimals, as in currency
    // - by design, this method is very fast and accurate
    procedure FromCurr(const value: Currency);
    /// fills from the text representation of a decimal value
    // - returns dsvValue or one of the dsvNan, dsvZero, dsvPosInf, dsvNegInf
    // special value indicator otherwise on succes
    // - returns dsvError on parsing failure
    function FromText(text: PUTF8Char; textlen: integer): TDecimal128SpecialValue; overload;
    /// fills from the text representation of a decimal value
    // - returns dsvValue or one of the dsvNan, dsvZero, dsvPosInf, dsvNegInf
    // special value indicator otherwise on succes
    // - returns dsvError on parsing failure
    function FromText(const text: RawUTF8): TDecimal128SpecialValue; overload;
    /// convert a variant into one Decimal128 value
    // - will first check for a TBSONVariant containing a betDecimal128 (e.g.
    // as retrieved via the ToVariant method)
    // - will recognize currency and VariantToInt64() stored values
    // - then will try to convert the variant from its string value, expecting
    // a floating-point text content
    // - returns TRUE if conversion was made, FALSE on any error
    function FromVariant(const value: variant): boolean;
    /// fills with a native floating-point value
    // - note that it doesn't make much sense to use this method: you should
    // rather use the native betFloat BSON format, with native double precision
    // - this method is just a wrapper around ExtendedToShort and ToText,
    // so you should provide the expected precision, from the actual storage
    // variable (you may specify e.g. SINGLE_PRECISION or EXTENDED_PRECISION if
    // you don't use a double kind of value)
    function FromFloat(const value: TSynExtended; precision: integer=0): boolean;
    /// fast bit-per-bit value comparison
    function Equals(const other: TDecimal128): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// converts the value to its string representation
    // - returns the number of AnsiChar written to Buffer
    function ToText(out Buffer: TDecimal128Str): integer; overload;
    /// converts this Decimal128 value to its string representation
    function ToText: RawUTF8; overload;
    /// converts this Decimal128 value to its string representation
    procedure ToText(var result: RawUTF8); overload;
    /// convert this Decimal128 value to its TBSONVariant custom variant value
    function ToVariant: variant; overload;
    /// convert this Decimal128 value to its TBSONVariant custom variant value
    procedure ToVariant(out result: variant); overload;
    /// converts this Decimal128 value to a floating-point value
    // - by design, some information may be lost during conversion
    // - note that it doesn't make much sense to use this method: you should
    // rather use the native betFloat BSON format, with native double precision
    function ToFloat: TSynExtended;
    /// converts this Decimal128 value to a fixed decimal value
    // - by design, some information may be lost during conversion, unless the
    // value has been stored previously via the FromCurr() method - in this
    // case, conversion is immediate and accurate
    function ToCurr: currency; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// converts this Decimal128 value to a fixed decimal value
    // - by design, some information may be lost during conversion, unless the
    // value has been stored previously via the FromCurr() method - in this
    // case, conversion is immediate and accurate
    procedure ToCurr(out result: currency); overload;
    /// converts this Decimal128 value to its string representation
    procedure AddText(W: TTextWriter);
  end;
  /// points to a 128-bit decimal value
  PDecimal128 = ^TDecimal128;

const
  /// the textual representation of the TDecimal128 special values
  DECIMAL128_SPECIAL_TEXT: array[TDecimal128SpecialValue] of RawUTF8 = (
    // dsvError, dsvValue, dsvNan, dsvZero, dsvPosInf, dsvNegInf, dsvMin, dsvMax
    '', '', 'NaN', '0', 'Infinity', '-Infinity',
    '-9.999999999999999999999999999999999E+6144',
     '9.999999999999999999999999999999999E+6144');

  BSON_DECIMAL128_HI_NAN        = $7c00000000000000;
  BSON_DECIMAL128_HI_INT64POS   = $3040000000000000; // 0 fixed decimals
  BSON_DECIMAL128_HI_INT64NEG   = $b040000000000000;
  BSON_DECIMAL128_HI_CURRPOS    = $3038000000000000; // 4 fixed decimals
  BSON_DECIMAL128_HI_CURRNEG    = $b038000000000000;
  BSON_DECIMAL128_EXPONENT_MAX  = 6111;
  BSON_DECIMAL128_EXPONENT_MIN  = -6176;
  BSON_DECIMAL128_EXPONENT_BIAS = 6176;
  BSON_DECIMAL128_MAX_DIGITS    = 34;

/// ready-to-be displayed text of a TDecimal128SpecialValue
function ToText(spec: TDecimal128SpecialValue): PShortString; overload;


type
  /// exception type used for BSON process
  EBSONException = class(ESynException);

  /// storage of a BSON binary document
  // - a specific type is defined for consistency with this unit classes
  // - binary content should follow the "int32 e_list #0" standard layout
  TBSONDocument = RawByteString;

  /// dynamic array of BSON binary document storage
  TBSONDocumentDynArray = array of TBSONDocument;

  /// element types for BSON internal representation
  TBSONElementType = (
    betEOF, betFloat, betString, betDoc, betArray, betBinary,
    betDeprecatedUndefined, betObjectID, betBoolean, betDateTime,
    betNull, betRegEx, betDeprecatedDbptr, betJS, betDeprecatedSymbol,
    betJSScope, betInt32, betTimestamp, betInt64, betDecimal128);

  /// points to an element type for BSON internal representation
  PBSONElementType = ^TBSONElementType;

  /// sub-types for betBinary element BSON internal representation
  TBSONElementBinaryType = (
    bbtGeneric, bbtFunction, bbtOldBinary, bbtOldUUID, bbtUUID, bbtMD5,
    bbtUser = $80);

  /// 24-bit storage, mapped as a 3 bytes buffer
  // - as used fo TBSONObjectID.MachineID and TBSONObjectID.Counter
  TBSON24 = record
    b1,b2,b3: byte;
  end;
  /// points to 24-bit storage, mapped as a 3 bytes buffer
  PBSON24 = ^TBSON24;

  /// BSON ObjectID 12-byte internal binary representation
  // - in MongoDB, documents stored in a collection require a unique _id field
  // that acts as a primary key: by default, it uses such a 12-byte ObjectID
  // - by design, sorting by _id: ObjectID is roughly equivalent to sorting by
  // creation time, so ease sharding and BTREE storage
  // - in our ODM, we rather use 64-bit genuine integer identifiers (TID),
  // as computed by an internal sequence or TSynUniqueIdentifierGenerator
  // - match betObjectID TBSONElementType
  {$A-}
  {$ifdef USERECORDWITHMETHODS}TBSONObjectID = record
    {$else}TBSONObjectID = object{$endif}
    /// big-endian 4-byte value representing the seconds since the Unix epoch
    // - time is expressed in Coordinated Universal Time (UTC), not local time
    UnixCreateTime: cardinal;
    /// 3-byte machine identifier
    // - ComputeNew will use a hash of ExeVersion.Host and ExeVersion.User
    MachineID: TBSON24;
    /// 2-byte process id
    // - ComputeNew will derivate it from MainThreadID
    ProcessID: word;
    /// 3-byte counter, starting with a random value
    // - used to avoid collision
    Counter: TBSON24;
    /// set all internal fields to zero
    procedure Init; {$ifdef HASINLINE} inline; {$endif}
    /// ObjectID content be filled with some unique values
    // - this implementation is thread-safe
    procedure ComputeNew;
    /// convert an hexadecimal string value into one ObjectID
    // - returns TRUE if conversion was made, FALSE on any error
    function FromText(const Text: RawUTF8): boolean; overload;
    /// convert an hexadecimal string value into one ObjectID
    // - returns TRUE if conversion was made, FALSE on any error
    function FromText(Text: PUTF8Char): boolean; overload;
    /// convert a variant into one ObjectID
    // - will first check for a TBSONVariant containing a betObjectID
    // - then will try to convert the variant from its string value, expecting
    // an hexadecimal text content
    // - returns TRUE if conversion was made, FALSE on any error
    function FromVariant(const value: variant): boolean;
    /// convert this ObjectID to its hexadecimal string value
    function ToText: RawUTF8; overload;
    /// convert this ObjectID to its hexadecimal string value
    procedure ToText(var result: RawUTF8); overload;
    /// convert this ObjectID to its TBSONVariant custom variant value
    function ToVariant: variant; overload;
    /// convert this ObjectID to its TBSONVariant custom variant value
    procedure ToVariant(var result: variant); overload;
    /// returns the timestamp portion of the ObjectId() object as a Delphi date
    // - time is expressed in Coordinated Universal Time (UTC), not local time
    // so you can compare it to NowUTC returned time
    function CreateDateTime: TDateTime;
    /// compare two Object IDs
    function Equal(const Another: TBSONObjectID): boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// compare two Object IDs, the second being stored in a TBSONVariant
    function Equal(const Another: variant): boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  /// points to a BSON ObjectID internal binary representation
  PBSONObjectID = ^TBSONObjectID;

  /// memory structure used for some special BSON storage as variant
  // - betObjectID kind will store a TBSONObjectID
  // - betBinary kind will store a BLOB content as RawByteString
  // - betDoc and betArray kind will store a BSON document, in its original
  // binary format as RawByteString (TBSONDocument)
  // - betDeprecatedDbptr, betJSScope, betTimestamp and betRegEx will store the
  // raw original BSON content as RawByteString
  // - betJS and betDeprecatedSymbol will store the UTF-8 encoded string
  // as a RawUTF8
  // - betDeprecatedUndefined or betMinKey/betMaxKey do not contain any data
  // - betDecimal128 will store the TDecimal128 16 bytes binary buffer
  // - warning: VBlob/VText use should match BSON_ELEMENTVARIANTMANAGED constant
  TBSONVariantData = packed record
    /// the variant type
    VType: TVarType;
    /// the kind of element stored
    case VKind: TBSONElementType of
    betObjectID: (
      {$IFDEF FPC} {$PUSH} {$ENDIF} {$HINTS OFF}
      // does not complain if Filler is declared but never used
      VFiller: array[1..SizeOf(TVarData)-SizeOf(TVarType)-SizeOf(TBSONElementType)
        -SizeOf(TBSONObjectID)] of byte;
      {$IFDEF FPC} {$POP} {$ELSE} {$HINTS ON} {$ENDIF}
      VObjectID: TBSONObjectID
    );
    betBinary, betDoc, betArray, betRegEx, betDeprecatedDbptr, betTimestamp,
    betJSScope, betDecimal128: (
      /// store the raw binary content as a RawByteString (or TBSONDocument for
      // betDoc/betArray, i.e. the "int32 e_list #0" standard layout)
      // - you have to use RawByteString(VBlob) when accessing this field
      // - e.g. for betRegEx, it will contain raw [cstring cstring] content
      VBlob: pointer;
    );
    betJS, betDeprecatedSymbol: (
      /// store here a RawUTF8 with the associated text
      // - you have to use RawUF8(VText) when accessing this field
      VText: pointer;
    );
  end;
  {$A+}

  /// points to memory structure used for some special BSON storage as variant
  PBSONVariantData = ^TBSONVariantData;

  /// custom variant type used to store some special BSON elements
  // - internal layout will follow TBSONVariantData
  // - handled kind of item are complex BSON types, like betObjectID, betBinary
  // or betDoc/betArray
  // - it will allow conversion to/from string (and to date for ObjectID)
  TBSONVariant = class(TSynInvokeableVariantType)
  protected
    function GetNewDoc(const BSONDoc: TBSONDocument): variant;
  public
    /// customization of JSON conversion into TBSONVariant kind of variants
    function TryJSONToVariant(var JSON: PUTF8Char; var Value: variant;
      EndOfObject: PUTF8Char): boolean; override;
    /// variant serialization will use modMongoStrict JSON-compatible mode
    procedure ToJSON(W: TTextWriter; const Value: variant; Escape: TTextWriterKind); override;
    /// handle type conversion
    // - only types processed by now are string/OleStr/UnicodeString/date
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    /// handle type conversion
    // - only types processed by now are string/OleStr/UnicodeString/date
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;
    /// clear the instance
    procedure Clear(var V: TVarData); override;
    /// copy one instance
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    /// compare two variant values
    // - handle comparison of any variant, including TBSONVariant, via a
    // temporary JSON conversion, and case-sensitive comparison
    // - it uses case-sensitive text (hexadecimal) comparison for betObjectID
    procedure Compare(const Left, Right: TVarData;
      var Relationship: TVarCompareResult); override;
    /// convert a TBSONDocument binary content into a TBSONVariant of kind
    // betDoc or betArray
    // - see also all BSONVariant() overloaded functions, which also create
    // a TBSONVariant betDoc instance
    procedure FromBSONDocument(const BSONDoc: TBSONDocument; var result: variant;
      Kind: TBSONElementType=betDoc);
    /// convert a BLOB binary content into a TBSONVariant of kind betBinary
    // - if Bin is '', will store a NULL variant
    procedure FromBinary(const Bin: RawByteString; BinType: TBSONElementBinaryType;
      var result: variant);
    /// convert a JSON content into a TBSONVariant of kind betDoc or betArray
    // - warning: the supplied JSON buffer will be modified in-place
    // - will create a plain variant value if the JSON doesn't start with [ or {
    procedure FromJSON(json: PUTF8Char; var result: variant);
    /// returns TRUE if the supplied variant stores the supplied BSON kind of value
    function IsOfKind(const V: variant; Kind: TBSONElementType): boolean;
    /// retrieve a betBinary content stored in a TBSONVariant instance
    // - returns TRUE if the supplied variant is a betBinary, and set the
    // binary value into the supplied Blob variable
    // - returns FALSE otherwise
    function ToBlob(const V: Variant; var Blob: RawByteString): boolean;
    /// convert a TBSONDocument binary content into a TBSONVariant of kind betDoc
    // - is the default property, so that you can write:
    // ! BSONVariantType[BSON(['BSON',_Arr(['awesome',5.05, 1986])])]
    // - see also all BSONVariant() overloaded functions, which also create
    // a TBSONVariant betDoc instance
    property NewDoc[const BSONDoc: TBSONDocument]: variant read GetNewDoc; default;
  end;

  /// define how betDoc/betArray BSON elements will be converted as variants
  // - by default a TBSONVariant custom type will be returned, containing the
  // raw BSON binary content of the embedded document or array
  // - asDocVariantPerValue or asDocVariantPerReference could be used to
  // create a tree of TDocVariant custom kind of variant, able to access
  // to its nested properties via late-binding (asDocVariantPerReference being
  // also much faster in some cases - but less safe - than asDocVariantPerValue)
  // - asDocVariantPerValue will set JSON_OPTIONS[false] settings:
  // ! [dvoReturnNullForUnknownProperty]
  // - asDocVariantPerReference will set JSON_OPTIONS[true]/JSON_OPTIONS_FAST
  // settings:
  // ! [dvoValueCopiedByReference,dvoReturnNullForUnknownProperty]
  // - asDocVariantInternNamesPerValue and asDocVariantInternNamesPerReference
  // will include dvoInternalNames to the TDocVariant.Options
  TBSONDocArrayConversion = (
    asBSONVariant, asDocVariantPerValue, asDocVariantPerReference,
    asDocVariantInternNamesPerValue, asDocVariantInternNamesPerReference);

  /// how TBSONElement.AddMongoJSON() method and AddMongoJSON() and
  // VariantSaveMongoJSON() functions will render their JSON content
  // - modMongoStrict and modNoMongo will follow the JSON RFC specifications
  // - modMongoShell will use a syntax incompatible with JSON RFC, but more
  // common to MongoDB daily use - as 'ObjectId()' or '{ field: /acme.*corp/i }'
  // - modMongoStrict will use the MongoDB Extended JSON syntax
  // - modNoMongo will serialize dates as ISO-8601 strings, ObjectID as hexadecimal
  // string and other MongoDB special objects in WrBase64() format
  // - see http://docs.mongodb.org/manual/reference/mongodb-extended-json
  TMongoJSONMode = (modNoMongo, modMongoStrict, modMongoShell);

  /// data structure used during BSON binary decoding of one BSON element
  // - will be retrieved by FromVariant() or FromNext()
  // - see http://bsonspec.org/#/specification
  // - this structure has been optimized to map the BSON binary content,
  // without any temporary memory allocation (the SAX way)
  {$ifdef USERECORDWITHMETHODS}TBSONElement = record
    {$else}TBSONElement = object {$endif}
  private
    /// used internally to set the TBSONElement content, once Kind has been set
    procedure FromBSON(bson: PByte);
  public
    /// index of this element in the original sequence list
    // - is correct only when the element has been reset before the parsing
    // loop, e.g.:
    // ! item.Index := -1; // so item.Index will count starting at 0
    // ! while item.FromNext(elem.Document) do
    // !   writeln(item.Index,' ',Item.Name,' ',Item.ValueBytes);
    Index: integer;
    /// the UTF-8 encoded name of this element
    Name: PUTF8Char;
    /// the name length (in chars) of this element
    NameLen: integer;
    /// the element type
    Kind: TBSONElementType;
    /// number of bytes in the BSON element
    // - will include the trailing #0 for string element
    ElementBytes: integer;
    /// pointer to the BSON element value
    // - is the raw value, without any parsing, e.g. points to a double value or
    // a document: "int32 e_list #0" standard layout (same as TBSONDocument)
    // - you may cast it for simple types:
    // ! PDouble(Element)^   PBoolean(Element)^        PInteger(Element)^
    // ! PInt64(Element)^    PBSONObjectID(Element)^   PDecimal128(Element)^
    // - or use the nested Data variant record to access more complex content
    // - warning: equals nil for betString/betJS after FromVariant()
    Element: pointer;
    /// depending on the Kind, will point to parsed complex sub-data
    // - since variable records can't have properties, we nest this information
    // within this main Data variable record
    // - not all Kind are handled here, only any complex data
    Data: record
    case TBSONElementType of
    betFloat, betBoolean, betInt32, betDateTime, betInt64: (
      /// this variable is not to be used directly, but for some internal
      // temporary storage, e.g. with FromVariant()
      // - use P*(Element)^ typecast instead
      InternalStorage: Int64;
    );
    betString, betJS: (
      /// points to the #0 ending string
      Text: PUTF8Char;
      /// number of bytes in Text (excluding trailing #0)
      TextLen: integer;
    );
    betDoc, betArray: (
      /// points to a "e_list #0" standard layout
      DocList: PByte;
    );
    betBinary: (
      /// points to the binary content
      Blob: pointer;
      /// number of bytes in Blob
      BlobLen: integer;
      /// corresponding sub-type of this Blob
      BlobSubType: TBSONElementBinaryType;
    );
    betRegEx: (
      RegEx: PUTF8Char;
      RegExLen: integer;
      RegExOptions: PUTF8Char;
      RegExOptionsLen: integer;
    );
    betJSScope: (
      JavaScript: PUTF8Char;
      JavaScriptLen: integer;
      ScopeDocument: PByte;
    );
    betTimestamp: (
      { map InternalStorage: Int64 }
      time_t: cardinal;
      ordinal: cardinal;
    );
    end;
    /// fill a BSON Element structure from a variant content and associated name
    // - perform the reverse conversion as made with ToVariant()
    // - since the result won't store any data but points to the original binary
    // content, the supplied Name/Value instances should remain available as long as
    // you will access to the result content
    // - aName here is just for conveniency, and could be left void
    // - supplied aTemp variable will be used for temporary storage, private to
    // this initialized TBSONElement
    procedure FromVariant(const aName: RawUTF8; const aValue: Variant;
      var aTemp: RawByteString);
    /// fill a BSON Element structure from a BSON document
    // - will check the document length then set Kind := betDoc and Data.DocList
    // - will return TRUE if the supplied doc has a valid length, FALSE otherwise
    // - you can later on use DocItemToVariant, DocItemToRawUTF8 or
    // DocItemToInteger methods
    // - the supplied "doc" variable should remain available until you are done
    // with this TBSONElement wrapper
    function FromDocument(const doc: TBSONDocument): boolean;
    /// fill a BSON Element structure from a BSON encoded binary buffer list
    // - parse the next BSON element: BSON parameter should point to the
    // "e_list" of the "int32 e_list #0" BSON document
    // - will decode the supplied binary buffer into the BSON element structure,
    // then it will let BSON point to the next element, and return TRUE
    // - returns FALSE when you reached betEOF, so that you can use it in a loop,
    // and retrieve all the content as consecutive events, without any memory
    // allocation (the SAX way):
    // ! var bson: PByte;
    // !     item: TBSONElement;
    // ! ...
    // ! BSONParseLength(bson);
    // ! while item.FromNext(bson) do
    // !   writeln(item.Name);
    // - will raise an EBSONException if BSON content is not correct
    // - as an alternative, consider using TBSONIterator, which wrap both a
    // PByte and a TBSONElement into one convenient item
    function FromNext(var BSON: PByte): boolean;
    /// search for a given name in a BSON encoded binary buffer list
    // - BSON parameter should point to the first "e_list" item of the
    // "int32 e_list #0" BSON document
    // - returns false if the item was not found (with case-insensitive search)
    // - otherwise returns TRUE and the matching element content has been
    // decoded within this TBSONElement structure
    function FromSearch(BSON: PByte; const aName: RawUTF8): boolean;
    /// convert a BSON element, as retrieved by TBSONElement.FromNext(),
    // into a variant
    // - it will return either standard variant values, or TBSONVariant custom type
    // for most complex kind of elements (see TBSONVariantData type definition)
    // - note that betString types will be stored as RawUTF8 varString
    // - by default, it will return TBSONVariant custom variants for documents or
    // arrays - but if storeDocArrayAsDocVariant is set, it will return a
    // TDocVariant custom kind of variant, able to access to its nested
    // properties via late-binding
    function ToVariant(DocArrayConversion: TBSONDocArrayConversion=asBSONVariant): variant; overload;
    /// convert a BSON element, as retrieved by TBSONElement.FromNext(),
    // into a variant
    // - same as the other ToVariant() overloaded function, but avoiding a copy
    // of the resulting variant
    procedure ToVariant(var result: variant;
      DocArrayConversion: TBSONDocArrayConversion=asBSONVariant); overload;
    /// convert a BSON element into an UTF-8 string
    // - any complex types (e.g. nested documents) will be converted via a
    // variant
    function ToRawUTF8: RawUTF8;
    /// convert a BSON element into an integer value
    // - will work only for betBoolean/betInt32/betInt64 types
    // - any other kind of values will return the supplied default value
    function ToInteger(const default: Int64=0): Int64;
    /// search a variant property value within the BSON element as document
    // - returns true if aName has been found as property in the BSON element,
    // and fills aValue with the corresponding value
    // - returns false if aName was not found, or if Kind is not betDoc or betArray
    function DocItemToVariant(const aName: RawUTF8; var aValue: variant;
      DocArrayConversion: TBSONDocArrayConversion=asBSONVariant): boolean;
    /// search an UTF-8 property value within the BSON element as document
    // - returns the value if aName has been found as property in the BSON element
    // - returns '' if aName was not found, or if Kind is not betDoc or betArray
    function DocItemToRawUTF8(const aName: RawUTF8): RawUTF8;
    /// search an integer property value within the BSON element as document
    // - returns the value if aName has been found as property in the BSON element
    // - returns default if aName was not found, or if Kind is not betDoc or betArray
    function DocItemToInteger(const aName: RawUTF8; const default: Int64=0): Int64;
    /// convert a BSON element, as retrieved by TBSONElement.FromNext(), into
    // its JSON representation
    // - this method will use by default the MongoDB Extended JSON syntax for
    // specific MongoDB objects but you may use modMongoShell if needed
    // - will raise an EBSONException if element is not correct
    procedure AddMongoJSON(W: TTextWriter; Mode: TMongoJSONMode=modMongoStrict); overload;
  end;
  PBSONElement = ^TBSONElement;

  /// data structure used for iterating over a BSON binary buffer
  // - is just a wrapper around a PByte value, to be used with a TBSONDocument
  {$ifdef USERECORDWITHMETHODS}TBSONIterator = record
    {$else}TBSONIterator = object {$endif}
  private
    fBson: PByte;
  public
    /// map the current item, after the Next method did return TRUE
    // - map the global document, after Init() but before the first Next call
    Item: TBSONElement;
    /// initialize the iteration on the supplied BSON document
    // - will check the document length and returns TRUE if it is correct
    // - only accepted kind are betDoc and betArray (but not used later)
    // - you can then use the Next method to iterate over the Item elements
    // - after this call, the Item property map to the global BSON document
    // (note that after any call to the Next method, Item will map the current
    // iterated value, and not the global BSON document any more)
    function Init(const doc: TBSONDocument; kind: TBSONElementType=betArray): boolean;
    /// will iterate on the BSON document
    // - returns TRUE if the item has been retrieved into the Item property
    // - returns FALSE if we reached the end of the supplied BSON buffer
    function Next: boolean;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  /// used to write the BSON context
  TBSONWriter = class(TFileBufferWriter)
  { note: inlining methods generates 70% SLOWER code due to inefficient compiler :( }
  protected
    fDocumentCount: integer;
    fDocument: array of record
      Offset: cardinal;
      Length: cardinal;
    end;
    fDocumentStack: integer;
    fDocumentStackOffset: TCardinalDynArray;
    fDocumentArray: integer;
    procedure WriteCollectionName(Flags: integer; const CollectionName: RawUTF8);
  public
    /// rewind the Stream to the position when Create() was called
    // - this will also reset the internal document offset table
    procedure CancelAll; override;

    /// write a boolean value
    procedure BSONWrite(const name: RawUTF8; const value: boolean); overload;
    /// write a floating point value
    procedure BSONWrite(const name: RawUTF8; const value: Double); overload;
    /// write a 32 bit integer value
    procedure BSONWrite(const name: RawUTF8; const value: integer); overload;
    /// write a 64 bit integer value
    procedure BSONWrite(const name: RawUTF8; const value: Int64); overload;
    /// write a string (UTF-8) value
    procedure BSONWrite(const name: RawUTF8; const value: RawUTF8; isJavaScript: boolean=false); overload;
    /// write a string (UTF-8) value from a memory buffer
    procedure BSONWrite(const name: RawUTF8; value: PUTF8Char); overload;
    /// write a string (UTF-8) value from a memory buffer
    procedure BSONWriteString(const name: RawUTF8; value: PUTF8Char; valueLen: integer);
    /// write a binary (BLOB) value
    procedure BSONWrite(const name: RawUTF8; Data: pointer; DataLen: integer); overload;
    /// write an ObjectID value
    procedure BSONWrite(const name: RawUTF8; const value: TBSONObjectID); overload;
    /// write a RegEx value
    procedure BSONWriteRegEx(const name: RawUTF8; const RegEx,Options: RawByteString);
    /// write a data/time value
    procedure BSONWriteDateTime(const name: RawUTF8; const value: TDateTime);
    /// write an element with no value
    // - elemType can be either betNull, betMinKey or betMaxKey
    procedure BSONWrite(const name: RawUTF8; elemtype: TBSONElementType); overload;
    /// write an element with no value
    procedure BSONWrite(const name: RawUTF8; const elem: TBSONElement); overload;
    /// write a BSONVariant instance value
    procedure BSONWrite(const name: RawUTF8; const bson: TBSONVariantData); overload;
    /// write a DocVariant instance value
    procedure BSONWrite(const name: RawUTF8; const doc: TDocVariantData); overload;
    /// write a TDecimal128 value
    procedure BSONWrite(const name: RawUTF8; const value: TDecimal128); overload;
    /// write a variant value
    // - handle simple types (numbers, strings...) and custom types (TDocVariant
    // and TBSONVariant, trying a translation to JSON for other custom types)
    procedure BSONWriteVariant(const name: RawUTF8; const value: variant); overload;
    /// write an open array (const Args: array of const) argument
    // - handle simple types (numbers, strings...) and custom types (TDocVariant)
    procedure BSONWrite(const name: RawUTF8; const value: TVarRec); overload;
    /// write a value from the supplied JSON content
    // - is able to handle any kind of values, including nested documents or
    // BSON extended syntax (if DoNotTryExtendedMongoSyntax=false)
    // - this method is used recursively by BSONWriteDocFromJSON(), and should
    // not be called directly
    // - will return JSON=nil in case of unexpected error in the supplied JSON
    procedure BSONWriteFromJSON(const name: RawUTF8; var JSON: PUTF8Char;
      EndOfObject: PUTF8Char; DoNotTryExtendedMongoSyntax: boolean=false);

    /// recursive writing of a BSON document or value from a TDocVariant
    // object or array, used e.g. by BSON(const doc: TDocVariantData) function
    // - caller should execute BSONAdjustDocumentsSize() on the resulting buffer
    // - this method will call BSONDocumentBegin/BSONDocumentEnd internally
    // - will raise an EBSONException if doc is not a valid TDocVariant or null
    // or if the resulting binary content is bigger than BSON_MAXDOCUMENTSIZE
    procedure BSONWriteDoc(const doc: TDocVariantData);
    /// write an object specified as name/value pairs as a BSON document
    // - data must be supplied two by two, as Name,Value pairs, e.g.
    // ! aBSONWriter.BSONWriteObject(['name','John','year',1972]);
    // - this method wil be faster than using a BSONWriteDoc(_ObjFast(...))
    procedure BSONWriteObject(const NameValuePairs: array of const);
    /// write a projection specified as fieldname:1 pairs as a BSON document
    procedure BSONWriteProjection(const FieldNamesCSV: RawUTF8);
    /// write an object as query parameter
    // - will handle all SQL operators, including IN (), IS NULL or LIKE
    // - see @http://docs.mongodb.org/manual/reference/operator/query
    // - inverted should be TRUE e.g. for a NOT ... expression
    // - returns TRUE on success, FALSE if the operator is not implemented yet
    function BSONWriteQueryOperator(name: RawUTF8; inverted: boolean;
      operator: TSynTableStatementOperator; const Value: variant): boolean;
    /// write one array item, i.e. the ASCII index name as text
    // - only one level of array should be used per TBSONWriter class
    procedure BSONWriteArray(const kind: TBSONElementType); overload;
    /// write an array specified as a list of items as a BSON document
    // - data must be supplied as a list of values e.g.
    // ! aBSONWriter.BSONWriteArray(['John',1972]);
    // - this method wil be faster than using a BSONWriteDoc(_ArrFast(...))
    procedure BSONWriteArray(const Items: array of const); overload;
    /// write an array of integers as a BSON Document
    procedure BSONWriteArrayOfInteger(const Integers: array of integer);
    /// write an array of integers as a BSON Document
    procedure BSONWriteArrayOfInt64(const Integers: array of Int64);
    /// write some BSON document from a supplied (extended) JSON array or object
    // - warning: the incoming JSON buffer will be modified in-place: so you
    // should make a private copy before running this method (see e.g. TSynTempBuffer)
    // - will handle only '{ ... }', '[ ... ]' or 'null' input, with the standard
    // strict JSON format, or BSON-like extensions, e.g. unquoted field names:
    // $ {id:10,doc:{name:"John",birthyear:1972}}
    // - if DoNotTryExtendedMongoSyntax is default FALSE, then the MongoDB Shell
    // syntax will also be recognized to create BSON custom types, like
    // $ new Date()   ObjectId()   MinKey   MaxKey  /<jRegex>/<jOptions>
    // see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
    // $ {id:new ObjectId(),doc:{name:"John",date:ISODate()}}
    // $ {name:"John",field:/acme.*corp/i}
    // - if DoNotTryExtendedMongoSyntax is TRUE, process may be slightly faster
    // - will create the BSON binary without any temporary TDocVariant storage
    function BSONWriteDocFromJSON(JSON: PUTF8Char; aEndOfObject: PUTF8Char;
      out Kind: TBSONElementType; DoNotTryExtendedMongoSyntax: boolean=false): PUTF8Char;

    /// to be called before a BSON document will be written
    // - each BSONDocumentBegin should be followed by its nested BSONDocumentEnd
    procedure BSONDocumentBegin; overload;
    /// to be called before a BSON document will be written
    // - each BSONDocumentBegin should be followed by its nested BSONDocumentEnd
    // - you could create a new BSON object by specifying a name and its
    // type, i.e. either betDoc or betArray
    procedure BSONDocumentBegin(const name: RawUTF8; kind: TBSONElementType=betDoc); overload;
    /// to be called before a BSON document will be written in an array
    // - only one level of array should be used per TBSONWriter class
    procedure BSONDocumentBeginInArray(const name: RawUTF8; kind: TBSONElementType=betDoc);
    /// to be called when a BSON document has been written
    // - it will store the current stream position into an internal array,
    // which will be written when you call AdjustDocumentsSize()
    // - you can optional specify how many nested documents should be closed,
    // and/or if it should not write an ending betEof item
    procedure BSONDocumentEnd(CloseNumber: integer=1; WriteEndingZero: boolean=true);
    /// after all content has been written, call this method on the resulting
    // memory buffer to store all document size as expected by the standard
    procedure BSONAdjustDocumentsSize(BSON: PByteArray); virtual;
    /// flush the content and return the whole binary encoded stream
    // - call BSONAdjustDocumentsSize() to adjust all internal document sizes
    // - expect the TBSONWriter instance to have been created as such:
    // ! TBSONWriter.Create(TRawByteStringStream);
    procedure ToBSONDocument(var result: TBSONDocument); virtual;
    /// flush the content and return the whole document as a TBSONVariant
    // - call ToBSONDocument() to adjust all internal document sizes
    // - expect the TBSONWriter instance to have been created as such:
    // ! TBSONWriter.Create(TRawByteStringStream);
    procedure ToBSONVariant(var result: variant; Kind: TBSONElementType=betDoc);
  end;


const
  /// fake BSON element type which compares lower than all other possible values
  // - element type sounds to be stored as shortint, so here $ff=-1<0=betEOF
  // - defined as an integer to circumvent a compilation issue with FPC trunk
  betMinKey = $ff;
  /// fake BSON element type which compares higher than all other possible values
  // - element type sounds to be stored as shortint, so here betInt64=$12<$7f
  // - defined as an integer to circumvent a compilation issue with FPC trunk
  betMaxKey = $7f;

  /// kind of elements which will store a RawByteString/RawUTF8 content
  // within its TBSONVariant kind
  // - i.e. TBSONVariantData.VBlob/VText field is to be managed
  BSON_ELEMENTVARIANTMANAGED =
   [betBinary, betDoc, betArray, betRegEx, betDeprecatedDbptr, betTimestamp,
    betJSScope, betJS, betDeprecatedSymbol, betDecimal128];

  /// by definition, maximum MongoDB document size is 16 MB
  BSON_MAXDOCUMENTSIZE = 16*1024*1024;

  /// special JSON string content which will be used to store a betDeprecatedUndefined item
  // - *[false] is for strict JSON, *[true] for MongoDB Extended JSON
  BSON_JSON_UNDEFINED: array[boolean] of string[23] =
    ('{"$undefined":true}','undefined');
  /// special JSON string content which will be used to store a betMinKey item
  // - *[false] is for strict JSON, *[true] for MongoDB Extended JSON
  BSON_JSON_MINKEY: array[boolean] of string[15] = ('{"$minKey":1}','MinKey');
  /// special JSON string content which will be used to store a betMaxKey item
  // - *[false] is for strict JSON, *[true] for MongoDB Extended JSON
  BSON_JSON_MAXKEY: array[boolean] of string[15] = ('{"$maxKey":1}','MaxKey');
  /// special JSON patterns which will be used to format a betObjectID item
  // - *[false,*] is to be written before the hexadecimal ID, *[true,*] after
  BSON_JSON_OBJECTID: array[boolean,TMongoJSONMode] of string[15] = (
    ('"','{"$oid":"','ObjectId("'),('"','"}','")'));
  /// special JSON patterns which will be used to format a betBinary item
  // - *[false,*] is for strict JSON, *[true,*] for MongoDB Extended JSON
  BSON_JSON_BINARY: array[boolean,boolean] of string[15] = (
    ('{"$binary":"','","$type":"'),('BinData(',',"'));
  /// special JSON string content which will be used to store a betDeprecatedDbptr
  // - *[false,*] is for strict JSON, *[true,*] for MongoDB Extended JSON
  // - (not used by now for this deprecated content)
  BSON_JSON_DBREF: array[boolean,0..2] of string[15] = (
    ('{"$ref":"','","$id":"','"}'),('DBRef("','","','")'));
  /// special JSON string content which will be used to store a betRegEx
  BSON_JSON_REGEX: array[0..2] of string[15] =
    ('{"$regex":"','","$options":"','"}');
  /// special JSON patterns which will be used to format a betDateTime item
  // - *[*,false] is to be written before the date value, *[*,true] after
  BSON_JSON_DATE: array[TMongoJSONMode,boolean] of string[15] = (
    ('"','"'),('{"$date":"','"}'),('ISODate("','")'));
  /// special JSON patterns which will be used to format a betDecimal128 item
  // - *[false,*] is to be written before the decimal value, *[true,*] after
  BSON_JSON_DECIMAL: array[boolean,TMongoJSONMode] of string[23] = (
    ('"','{"$numberDecimal":"','NumberDecimal("'),('"','"}','")'));

var
  /// global TCustomVariantType used to register BSON variant types
  // - if you use this unit, both TDocVariant and TBSONVariant custom types
  // will be registered, since they are needed for any MongoDB / BSON process
  BSONVariantType: TBSONVariant;

/// ready-to-be displayed text of a TBSONElementType value
function ToText(kind: TBSONElementType): PShortString; overload;

/// create a TBSONVariant custom variant type containing a BSON Object ID
// - will be filled with some unique values, ready to create a new document key
// - will store a BSON element of betObjectID kind
function ObjectID: variant; overload;

/// create a TBSONVariant Object ID custom variant type from a supplied text
// - will raise an EBSONException if the supplied text is not valid hexadecimal
// - will set a BSON element of betObjectID kind
function ObjectID(const Hexa: RawUTF8): variant; overload;

/// convert a TBSONVariant Object ID custom variant into a TBSONObjectID
// - raise an exception if the supplied variant is not a TBSONVariant Object ID
function BSONObjectID(const aObjectID: variant): TBSONObjectID;

/// create a TBSONVariant JavaScript custom variant type from a supplied code
// - will set a BSON element of betJS kind
function JavaScript(const JS: RawUTF8): variant; overload;

/// create a TBSONVariant JavaScript and associated scope custom variant type
// from a supplied code and document
// - will set a BSON element of betJSScope kind
function JavaScript(const JS: RawUTF8; const Scope: TBSONDocument): variant; overload;

/// create a TBSONVariant Decimal128 from some text corresponding to
// a floating-point number
// - will store internally a TDecimal128 storage
function NumberDecimal(const Value: RawUTF8): variant; overload;

/// create a TBSONVariant Decimal128 from a currency fixed decimal
// - will store internally a TDecimal128 storage, with explictly 4 decimals
// - if you want to store some floating-point value, use plain BSON double format
function NumberDecimal(const Value: currency): variant; overload;

/// store some object content into BSON encoded binary
// - object will be initialized with data supplied two by two, as Name,Value
// pairs, e.g.:
// ! aBson := BSON(['name','John','year',1972]);
// - you can define nested arrays or objects as TDocVariant, e.g:
// ! aBSON := BSON(['bsonDat',_Arr(['awesome',5.05, 1986])]);
// - or you can specify nested arrays or objects with '['..']' or '{'..'}':
// ! aBSON := BSON(['BSON','[','awesome',5.05,1986,']'])
// ! u := BSONToJSON(BSON(['doc','{','name','John','year',1982,'}','id',123]));
// ! assert(u='{"doc":{"name":"John","year":1982},"id":123}');
// ! u := BSONToJSON(BSON(['doc','{','name','John','abc','[','a','b','c',']','}','id',123]));
// ! assert(u='{"doc":{"name":"John","abc":["a","b","c"]},"id":123}');
// - will create the BSON binary without any temporary TDocVariant storage
function BSON(const NameValuePairs: array of const): TBSONDocument; overload;

/// create a fields selector BSON document from a field names list
// - can be used via a TBSONVariant instance for the projection parameter of
// a TMongoRequestQuery, e.g.:
// ! BSONToJSON(BSONFieldSelector(['a','b','c']))='{"a":1,"b":1,"c":1}'
function BSONFieldSelector(const FieldNames: array of RawUTF8): TBSONDocument; overload;

/// create a fields selector BSON document from a CSV field names list
// - can be used via a TBSONVariant instance for the projection parameter of
// a TMongoRequestQuery, e.g.:
// ! BSONToJSON(BSONFieldSelector('a,b,c'))='{"a":1,"b":1,"c":1}'
function BSONFieldSelector(const FieldNamesCSV: RawUTF8): TBSONDocument; overload;

/// store some object content, supplied as (extended) JSON, into BSON encoded binary
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names:
// ! BSON('{id:10,doc:{name:"John",birthyear:1972}}');
// - MongoDB Shell syntax will also be recognized to create TBSONVariant, like
// ! new Date()   ObjectId()   MinKey   MaxKey  /<jRegex>/<jOptions>
// see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
// ! BSON('{id:new ObjectId(),doc:{name:"John",date:ISODate()}}');
// ! BSON('{name:"John",field:/acme.*corp/i}');
// - will create the BSON binary without any temporary TDocVariant storage, by
// calling JSONBufferToBSONDocument() on a temporary copy of the supplied JSON
function BSON(const JSON: RawUTF8; kind: PBSONElementType=nil): TBSONDocument; overload;
  {$ifndef ISDELPHI20092010}{$ifdef HASINLINE}inline;{$endif}{$endif}

/// store some object content, supplied as (extended) JSON and parameters,
// into BSON encoded binary
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names
// - MongoDB Shell syntax will also be recognized to create TBSONVariant, like
// ! new Date()   ObjectId()   MinKey   MaxKey  /<jRegex>/<jOptions>
// see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
// - typical use could be:
// ! BSON('{%:{$in:[?,?]}}',['type'],['food','snack']);
// ! BSON('{type:{$in:?}}',[],[_Arr(['food','snack'])]);
// ! BSON('{%:[?,?,?]}',['BSON'],['awesome',5.05,1986])
// ! BSON('{%:?}',['BSON'],[_Arr(['awesome',5.05,1986])])
// ! BSON('{name:?,field:/%/i}',['acme.*corp'],['John']);
// ! BSON('{id:new ObjectId(),doc:{name:?,date:ISODate(?)}}',[],['John',NowUTC]);
// - will create the BSON binary without any temporary TDocVariant storage,
// by calling JSONBufferToBSONDocument() on the generated JSON content
// - since all content will be transformed into JSON internally, use this
// method only if the supplied parameters are simple types, and identified
// explicitely via BSON-like extensions: any complex value (e.g. a TDateTime
// or a BSONVariant binary) won't be handled as expected - use the overloaded
// BSON() with explicit BSONVariant() name/value pairs instead
function BSON(const Format: RawUTF8; const Args,Params: array of const;
  kind: PBSONElementType=nil): TBSONDocument; overload;

/// store some TDocVariant custom variant content into BSON encoded binary
// - will write either a BSON object or array, depending of the internal
// layout of this TDocVariantData instance (i.e. Kind property value)
// - if supplied variant is not a TDocVariant, raise an EBSONException
function BSON(const doc: TDocVariantData): TBSONDocument; overload;

/// store an array of integer into BSON encoded binary
// - object will be initialized with data supplied e.g. as a TIntegerDynArray
function BSONFromIntegers(const Integers: array of integer): TBSONDocument;

/// store an array of 64 bit integer into BSON encoded binary
// - object will be initialized with data supplied e.g. as a TIntegerDynArray
function BSONFromInt64s(const Integers: array of Int64): TBSONDocument;

/// store some object content, supplied as (extended) JSON into BSON binary
// - warning: the supplied JSON buffer will be modified in-place, if necessary:
// so you should create a temporary copy before calling this function, or call
// BSON(const JSON: RawUTF8) function instead
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names
// - if DoNotTryExtendedMongoSyntax is FALSE, then MongoDB Shell syntax will
// also be recognized to create BSON custom values, like
// ! new Date()   ObjectId()   MinKey   MaxKey  /<jRegex>/<jOptions>
// see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
// ! BSON('{id:new ObjectId(),doc:{name:"John",date:ISODate()}}');
// ! BSON('{name:"John",field:/acme.*corp/i}');
// - will create the BSON binary without any temporary TDocVariant storage
// - will return the kind of BSON document created, i.e. either betDoc or betArray
function JSONBufferToBSONDocument(JSON: PUTF8Char; var doc: TBSONDocument;
  DoNotTryExtendedMongoSyntax: boolean=false): TBSONElementType;

/// store one JSON array into an array of BSON binary
// - since BSON documents are limited to 16 MB by design, this function
// will allow to process huge data content, as soon as it is provided as array
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names
// - if DoNotTryExtendedMongoSyntax is FALSE, then MongoDB Shell syntax will
// be recognized to create BSON custom values - but it will be slightly slower
function JSONBufferToBSONArray(JSON: PUTF8Char; out docs: TBSONDocumentDynArray;
  DoNotTryExtendedMongoSyntax: boolean=false): boolean;

/// store some object content into a TBSONVariant betDoc type instance
// - object will be initialized with data supplied two by two, as Name,Value
// pairs, as expected by the corresponding overloaded BSON() function
function BSONVariant(const NameValuePairs: array of const): variant; overload;

/// create a fields selector BSON document from a field names list
// - can be used for the projection parameter of a TMongoRequestQuery, e.g.:
// ! VariantToJSON(BSONVariantFieldSelector(['a','b','c']))='{"a":1,"b":1,"c":1}'
function BSONVariantFieldSelector(const FieldNames: array of RawUTF8): variant; overload;

/// create a fields selector BSON document from a CSV field names list
// - can be used for the projection parameter of a TMongoRequestQuery, e.g.:
// ! VariantToJSON(BSONVariantFieldSelector('a,b,c'))='{"a":1,"b":1,"c":1}'
function BSONVariantFieldSelector(const FieldNamesCSV: RawUTF8): variant; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// store some object content, supplied as (extended) JSON, into a TBSONVariant
// betDoc type instance
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, as with the overloaded BSON() function
function BSONVariant(const JSON: RawUTF8): variant; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// store some object content, supplied as (extended) JSON, into a TBSONVariant
// betDoc type instance
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, as with the overloaded BSON() function
// - warning: this overloaded method will mofify the supplied JSON buffer
// in-place: you can use the overloaded BSONVariant(const JSON: RawUTF8) function
// instead if you do not want to modify the input buffer content
procedure BSONVariant(JSON: PUTF8Char; var result: variant); overload;

/// store some object content, supplied as (extended) JSON and parameters,
// into a TBSONVariant betDoc type instance
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, as with the overloaded BSON() function
function BSONVariant(const Format: RawUTF8; const Args,Params: array of const): variant; overload;

/// convert a TDocVariant variant into a TBSONVariant betDoc type instance
function BSONVariant(doc: TDocVariantData): variant; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// store an array of integer into a TBSONVariant betArray type instance
// - object will be initialized with data supplied e.g. as a TIntegerDynArray
function BSONVariantFromIntegers(const Integers: array of integer): variant;

/// store an array of 64 bit integer into a TBSONVariant betArray type instance
// - object will be initialized with data supplied e.g. as a TIntegerDynArray
function BSONVariantFromInt64s(const Integers: array of Int64): variant;

/// parse the header of a BSON encoded binary buffer, and return its length
// - BSON should point to a "int32 e_list #0" BSON document (like TBSONDocument)
// - if ExpectedBSONLen is set, this function will check that the supplied
// BSON content "int32" length matches the supplied value, and raise an
// EBSONException if this comparison fails
// - as an alternative, consider using TBSONIterator, which wrap both a PByte
// and a TBSONElement into one convenient item
function BSONParseLength(var BSON: PByte; ExpectedBSONLen: integer=0): integer;

/// parse the next element in supplied BSON encoded binary buffer list
// - BSON should point to the "e_list" of the "int32 e_list #0" BSON document
// - will decode the supplied binary buffer as a variant, then it will let BSON
// point to the next element, and return TRUE
// - returns FALSE when you reached betEOF, so that you can use it in a loop:
// ! var bson: PByte;
// !     name: RawUTF8;
// !     value: variant;
// ! ...
// ! BSONParseLength(bson);
// ! while BSONParseNextElement(bson,name,value) do
// !   writeln(name,':',value);
// - by default, it will return TBSONVariant custom variants for documents or
// arrays - but if storeDocArrayAsDocVariant is set, it will return a
// TDocVariant custom kind of variant, able to access to its nested
// properties via late-binding
// - if you want to parse a BSON list as fast as possible, you should better use
// TBSONElement.FromNext() which avoid any memory allocation (the SAX way) - in
// fact, this function is just a wrapper around TBSONElement.FromNext + ToVariant
// - as an alternative, consider using TBSONIterator, which wrap both a PByte
// and a TBSONElement into one convenient item
function BSONParseNextElement(var BSON: PByte; var name: RawUTF8; var element: variant;
  DocArrayConversion: TBSONDocArrayConversion=asBSONVariant): boolean;

/// search for a property by number in a a supplied BSON encoded binary buffer
// - BSON should point to a "int32 e_list #0" BSON document (like TBSONDocument)
// - returns FALSE if the list has too few elements (starting at index 0)
// - otherwise, returns TRUE then let item point to the corresponding element
function BSONPerIndexElement(BSON: PByte; index: integer; var item: TBSONElement): boolean;

/// convert a BSON document into a TDocVariant variant instance
// - BSON should point to a "int32 e_list #0" BSON document
// - if ExpectedBSONLen is set, this function will check that the supplied
// BSON content "int32" length matches the supplied value
// - by definition, asBSONVariant is not allowed as Option value
procedure BSONToDoc(BSON: PByte; var Result: Variant; ExpectedBSONLen: integer=0;
  Option: TBSONDocArrayConversion=asDocVariantPerReference);

/// convert a TBSONDocument into a TDocVariant variant instance
// - BSON should be valid BSON document (length will be checked against expected
// "int32 e_list #0" binary layout)
// - by definition, asBSONVariant is not allowed as Option value
function BSONDocumentToDoc(const BSON: TBSONDocument;
  Option: TBSONDocArrayConversion=asDocVariantPerReference): variant;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a BSON document into its JSON representation
// - BSON should point to a "int32 e_list #0" BSON document
// - Kind should be either betDoc or betArray
// - if ExpectedBSONLen is set, this function will check that the supplied
// BSON content "int32" length matches the supplied value
// - this function will use by default the MongoDB Extended JSON syntax for
// specific MongoDB objects but you may use modMongoShell if needed
function BSONToJSON(BSON: PByte; Kind: TBSONElementType;
   ExpectedBSONLen: integer=0; Mode: TMongoJSONMode=modMongoStrict): RawUTF8;

/// convert a TBSONDocument into its JSON representation
// - BSON should be valid BSON document (length will be checked against expected
// "int32 e_list #0" binary layout)
// - this function will use by default the MongoDB Extended JSON syntax for
// specific MongoDB objects but you may use modMongoShell if needed
function BSONDocumentToJSON(const BSON: TBSONDocument;
  Mode: TMongoJSONMode=modMongoStrict): RawUTF8;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a BSON list of elements into its JSON representation
// - BSON should point to the "e_list" of the "int32 e_list #0" BSON document,
// i.e. the item data as expected by TBSONElement.FromNext()
// - this function will use by default the MongoDB Extended JSON syntax for
// specific MongoDB objects but you may use modMongoShell if needed
procedure BSONListToJSON(BSONList: PByte; Kind: TBSONElementType; W: TTextWriter;
  Mode: TMongoJSONMode=modMongoStrict);

/// convert any kind of BSON/JSON element, encoded as variant, into JSON
// - this function will use by default the MongoDB Extended JSON syntax for
// specific MongoDB objects but you may use modMongoShell if needed
procedure AddMongoJSON(const Value: variant; W: TTextWriter;
  Mode: TMongoJSONMode=modMongoStrict); overload;

/// convert any kind of BSON/JSON element, encoded as variant, into JSON
// - in addition to default modMongoStrict as rendered by VariantSaveJSON(),
// this function can render the supplied variant with the Mongo Shell syntax
// or even raw JSON content
function VariantSaveMongoJSON(const Value: variant; Mode: TMongoJSONMode): RawUTF8;




{ ************ MongoDB Client }

const
  /// MongoDB server default IP port
  MONGODB_DEFAULTPORT = 27017;

type
  /// exception type used for MongoDB process
  EMongoException = class(ESynException);

  /// the available MongoDB driver Request Opcodes
  // - opReply: database reply to a client request - ResponseTo shall be set
  // - opMsgOld: generic msg command followed by a string (deprecated)
  // - opUpdate: update document
  // - opInsert: insert new document
  // - opQuery: query a collection
  // - opGetMore: get more data from a previous query
  // - opDelete: delete documents
  // - opKillCursors: notify database client is done with a cursor
  // - opMsg: new OP_MSG layout introduced in MongoDB 3.6
  TMongoOperation = (
    opReply, opMsgOld, opUpdate, opInsert, opQuery, opGetMore, opDelete,
    opKillCursors, opMsg);

  /// define how an opUpdate operation will behave
  // - if mufUpsert is set, the database will insert the supplied object into
  // the collection if no matching document is found
  // - if mufMultiUpdate is set, the database will update all matching objects
  // in the collection; otherwise (by default) only updates first matching doc
  TMongoUpdateFlag =
    (mufUpsert, mufMultiUpdate);

  /// define how a TMongoRequestUpdate message will behave
  TMongoUpdateFlags = set of TMongoUpdateFlag;

  /// define how an opInsert operation will behave
  // - if mifContinueOnError is set, the database will not stop processing a
  // bulk insert if one fails (e.g. due to duplicate IDs); this makes bulk
  // insert behave similarly to a series of single inserts, except lastError
  // will be set if any insert fails, not just the last one - if multiple
  // errors occur, only the most recent will be reported by getLastError
  TMongoInsertFlag =
    (mifContinueOnError);

  /// define how a TMongoRequestInsert message will behave
  TMongoInsertFlags = set of TMongoInsertFlag;

  /// define how an opDelete operation will behave
  // - if mdfSingleRemove is set, the database will remove only the first
  // matching document in the collection. Otherwise (by default) all matching
  // documents will be removed
  TMongoDeleteFlag =
    (mdfSingleRemove);

  /// define how a TMongoRequestDelete message will behave
  TMongoDeleteFlags = set of TMongoDeleteFlag;

  /// define how an opQuery operation will behave
  // - if mqfTailableCursor is set, cursor is not closed when the last data
  // is retrieved
  // - if mqfSlaveOk is set, it will allow query of replica slave; normally
  // this returns an error except for namespace "local"
  // - mqfOplogReplay is internal replication use only - driver should not set
  // - if mqfNoCursorTimeout is set, the server normally does not times out
  // idle cursors after an inactivity period (10 minutes) to prevent
  // excess memory use
  // - if mqfAwaitData is to use with TailableCursor. If we are at the end
  // of the data, block for a while rather than returning no data. After a
  // timeout period, we do return as normal
  // - if mqfExhaust is set, stream the data down full blast in multiple "more"
  // packages, on the assumption that the client will fully read all data queried
  // - if mqfPartial is set, it will get partial results from a mongos if
  // some shards are down (instead of throwing an error)
  TMongoQueryFlag =
    (mqfTailableCursor=1, mqfSlaveOk, mqfOplogReplay, mqfNoCursorTimeout,
     mqfAwaitData, mqfExhaust, mqfPartial);

  /// define how a TMongoRequestQuery message will behave
  TMongoQueryFlags = set of TMongoQueryFlag;

  /// abstract class used to create MongoDB Wire Protocol client messages
  // - see http://docs.mongodb.org/meta-driver/latest/legacy/mongodb-wire-protocol
  // - this class is not tight to the connection class itself (which is one
  // known limitation of TMongoWire for instance)
  TMongoRequest = class(TBSONWriter)
  protected
    fRequestID: integer;
    fResponseTo: integer;
    fRequestOpCode: TMongoOperation;
    fDatabaseName,
    fCollectionName,
    fFullCollectionName: RawUTF8;
    fBSONDocument: TBSONDocument;
  public
    /// write a standard Message Header for MongoDB client
    // - opCode is the type of the message
    // - requestID  is a client or database-generated identifier that uniquely
    // identifies this message: in case of opQuery or opGetMore messages, it will
    // be sent in the responseTo field from the database
    // - responseTo is the requestID taken from previous opQuery or opGetMore
    constructor Create(const FullCollectionName: RawUTF8;
      opCode: TMongoOperation; requestID, responseTo: integer); reintroduce;
    /// append a query parameter as a BSON document
    // - param can be a TDocVariant, e.g. created with:
    // ! _JsonFast('{name:"John",age:{$gt:21}}');
    // ! _JsonFastFmt('{name:?,age:{$gt:?}}',[],['John',21]);
    // ! _JsonFastFmt('{name:?,field:/%/i}',['acme.*corp'],['John']);
    // - param can be a TBSONVariant containing a TBSONDocument raw binary block
    // created e.g. from:
    // ! BSONVariant(['BSON',_Arr(['awesome',5.05, 1986])])
    // ! BSONVariantType[BSON(['BSON',_Arr(['awesome',5.05, 1986])])]
    // - if param is null, it will append a void document
    // - if param is a string, it will be converted as expected by most
    // database commands, e.g.
    // ! TMongoRequestQuery.Create('admin.$cmd','buildinfo',[],1)
    // will query   { buildinfo: 1 }  to the  admin.$cmd  collection, i.e.
    // $ admin.$cmd.findOne( { buildinfo: 1 } )
    procedure BSONWriteParam(const paramDoc: variant);
    /// flush the content and return the whole binary encoded stream
    // - expect the TBSONWriter instance to have been created with reintroduced
    // Create() specific constructors inheriting from this TMongoRequest class
    // - this overridden version will adjust the size in the message header
    procedure ToBSONDocument(var result: TBSONDocument); override;
    /// write the main parameters of the request as JSON
    procedure ToJSON(W: TTextWriter; Mode: TMongoJSONMode); overload; virtual;
    /// write the main parameters of the request as JSON
    function ToJSON(Mode: TMongoJSONMode): RawUTF8; overload;
    /// identify the message, after call to any reintroduced Create() constructor
    property MongoRequestID: integer read fRequestID;
    /// the associated full collection name, e.g. 'db.test'
    property FullCollectionName: RawUTF8 read fFullCollectionName;
    /// the associated full collection name, e.g. 'db'
    property DatabaseName: RawUTF8 read fDatabaseName;
    /// the associated full collection name, e.g. 'test'
    property CollectionName: RawUTF8 read fCollectionName;
    /// the message operation code
    // - should be either opUpdate, opInsert, opQuery, opGetMore, opDelete
    // or opKillCursors, depending on the TMongoRequest* class instantiated
    property MongoRequestOpCode: TMongoOperation read fRequestOpCode;
  end;

  /// a MongoDB client abstract ancestor which is able to create a BULK
  // command message for MongoDB >= 2.6 instead of older dedicated Wire messages
  TMongoRequestWritable = class(TMongoRequest)
  protected
  public
  end;

  /// a MongoDB client message to update a document in a collection
  TMongoRequestUpdate = class(TMongoRequestWritable)
  protected
    fSelector, fUpdate: TVarData;
  public
    /// initialize a MongoDB client message to update a document in a collection
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - how the update will be processed can be customized via Flags
    // - Selector is the BSON document query to select the document, supplied as
    // TDocVariant - i.e. created via _JsonFast() or _JsonFastFmt() - or as
    // TBSONVariant - i.e. created via BSONVariant() - or null if all documents
    // are to be updated
    // - Update is the BSON document specification of the update to perform,
    // supplied as TDocVariant or TBSONVariant
    // - there is no response to an opUpdate message
    constructor Create(const FullCollectionName: RawUTF8;
      const Selector, Update: variant; Flags: TMongoUpdateFlags=[]); reintroduce;
    /// write the main parameters of the request as JSON
    procedure ToJSON(W: TTextWriter; Mode: TMongoJSONMode); override;
  end;

  /// a MongoDB client message to insert one or more documents in a collection
  TMongoRequestInsert = class(TMongoRequestWritable)
  public
    /// initialize a MongoDB client message to insert one or more documents in
    // a collection, supplied as variants
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - Documents is an array of TDocVariant or TBSONVariant - i.e. created via
    // _JsonFast() _JsonFastFmt() or BSONVariant()
    // - there is no response to an opInsert message
    constructor Create(const FullCollectionName: RawUTF8;
      const Documents: array of variant; Flags: TMongoInsertFlags=[]); reintroduce; overload;
    /// initialize a MongoDB client message to insert one or more documents in
    // a collection, supplied as raw BSON binary
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - Documents is the low-level concatenation of BSON documents, created
    // e.g. with a TBSONWriter stream
    // - there is no response to an opInsert message
    constructor Create(const FullCollectionName: RawUTF8;
      const Documents: TBSONDocument; Flags: TMongoInsertFlags=[]); reintroduce; overload;
    /// initialize a MongoDB client message to insert one or more documents in
    // a collection, supplied as JSON objects
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - JSONDocuments is an array of JSON objects
    // - there is no response to an opInsert message
    // - warning: JSONDocuments[] buffer will be modified in-place during
    // parsing, so a private copy may have to be made by the caller
    constructor Create(const FullCollectionName: RawUTF8;
      const JSONDocuments: array of PUTF8Char; Flags: TMongoInsertFlags=[]); reintroduce; overload;
  end;

  /// a MongoDB client message to delete one or more documents in a collection
  TMongoRequestDelete = class(TMongoRequestWritable)
  protected
    fQuery: TVarData;
  public
    /// initialize a MongoDB client message to delete one or more documents in
    // a collection
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - Selector is the BSON document query to select the document, supplied as
    // TDocVariant - i.e. created via _JsonFast() or _JsonFastFmt() - or as
    // TBSONVariant - i.e. created via BSONVariant() - or null if all documents
    // are to be deleted
    // - warning: CreateDelete('db.coll',null) can be expensive so you should
    // better drop the whole collection
    // - there is no response to an opDelete message
    constructor Create(const FullCollectionName: RawUTF8;
      const Selector: variant; Flags: TMongoDeleteFlags=[]); reintroduce;
    /// write the main parameters of the request as JSON
    procedure ToJSON(W: TTextWriter; Mode: TMongoJSONMode); override;
  end;

  /// a MongoDB client message to query one or more documents in a collection
  TMongoRequestQuery = class(TMongoRequest)
  protected
    fNumberToReturn,fNumberToSkip: integer;
    fQuery, fReturnFieldsSelector: TVarData;
  public
    /// initialize a MongoDB client message to query one or more documents in
    // a collection from a specified Cursor identifier
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - Query is the BSON document query to select the document, supplied as
    // TDocVariant - i.e. created via _JsonFast() or _JsonFastFmt() - or null
    // if all documents are to be retrieved - for instance:
    // ! _JsonFast('{name:"John",age:{$gt:21}}');
    // ! _JsonFastFmt('{name:?,age:{$gt:?}}',[],['John',21]);
    // ! _JsonFastFmt('{name:?,field:/%/i}',['acme.*corp'],['John']);
    // - if Query is a string, it will be converted as expected by most
    // database commands, e.g.
    // $ TMongoRequestQuery.Create('admin.$cmd','buildinfo',[],1)
    // will query   { buildinfo: 1 }  to the  admin.$cmd  collection, i.e.
    // $ admin.$cmd.findOne( { buildinfo: 1 } )
    // - Query can also be a TBSONVariant, e.g. created with:
    // ! BSONVariant('{name:?,age:{$gt:?}}',[],['John',21])
    // - ReturnFieldsSelector is an optional selector (set to null if not
    // applicable) as a BSON document that limits the fields in the returned
    // documents, supplied as TDocVariant or TBSONVariant - e.g. created via:
    // ! BSONVariantFieldSelector('a,b,c');
    // ! BSONVariantFieldSelector(['a','b','c']);
    // ! BSONVariant('{a:1,b:1,c:1}');
    // ! _JsonFast('{a:1,b:1,c:1}');
    // - if ReturnFieldsSelector is a string, it will be converted into
    // $ { ReturnFieldsSelector: 1 }
    constructor Create(const FullCollectionName: RawUTF8;
      const Query, ReturnFieldsSelector: variant; NumberToReturn: integer;
      NumberToSkip: integer=0; Flags: TMongoQueryFlags=[]); reintroduce;
    /// write the main parameters of the request as JSON
    procedure ToJSON(W: TTextWriter; Mode: TMongoJSONMode); override;
    /// retrieve the NumberToReturn parameter as set to the constructor
    property NumberToReturn: integer read fNumberToReturn;
    /// retrieve the NumberToSkip parameter as set to the constructor
    property NumberToSkip: integer read fNumberToSkip;
  end;

  /// a MongoDB client message to continue the query of one or more documents
  // in a collection, after a TMongoRequestQuery message
  TMongoRequestGetMore = class(TMongoRequest)
  public
    /// initialize a MongoDB client message to continue the query of one or more
    // documents in a collection, after a opQuery / TMongoRequestQuery message
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - you can specify the number of documents to return (e.g. from previous
    // opQuery response)
    // - CursorID should have been retrieved within an opReply message from the
    // database
    constructor Create(const FullCollectionName: RawUTF8;
      NumberToReturn: integer; CursorID: Int64); reintroduce;
  end;

  /// a MongoDB client message to close one or more active cursors
  TMongoRequestKillCursor = class(TMongoRequest)
  protected
    fCursors: TInt64DynArray;
  public
    /// initialize a MongoDB client message to close one or more active cursors
    // in the database
    // - it is mandatory to ensure that database resources are reclaimed by
    // the client at the end of the query
    // - if a cursor is read until exhausted (read until opQuery or opGetMore
    // returns zero for the CursorId), there is no need to kill the cursor
    // - there is no response to an opKillCursor message
    constructor Create(const FullCollectionName: RawUTF8;
      const CursorIDs: array of Int64); reintroduce;
    /// write the main parameters of the request as JSON
    procedure ToJSON(W: TTextWriter; Mode: TMongoJSONMode); override;
  end;


  /// used to store the binary raw data a database response to a
  // TMongoRequestQuery / TMongoRequestGetMore client message
  TMongoReply = RawByteString;

  /// define an opReply message execution content
  // - mrfCursorNotFound will be set when getMore is called but the cursor id
  // is not valid at the server; returned with zero results
  // - mrfQueryFailure is set when the query failed - results consist of one
  // document containing an "$err" field describing the failure
  // - mrfShardConfigStale should not be used by client, just by Mongos
  // - mrfAwaitCapable is set when the server supports the AwaitData Query
  // option (always set since Mongod version 1.6)
  TMongoReplyCursorFlag = (
    mrfCursorNotFound, mrfQueryFailure, mrfShardConfigStale,
    mrfAwaitCapable);

  /// define a TMongoReplyCursor message execution content
  TMongoReplyCursorFlags = set of TMongoReplyCursorFlag;

  /// internal low-level binary structure mapping all message headers
  TMongoWireHeader = packed record
    /// total message length, including the header
    MessageLength: integer;
    /// identifier of this message
    RequestID: integer;
    /// retrieve the RequestID from the original request
    ResponseTo: integer;
    /// low-level code of the message
    // - GetReply() will map it to a high-level TMongoOperation
    OpCode: integer;
  end;
  PMongoWireHeader = ^TMongoWireHeader;

  /// internal low-level binary structure mapping the TMongoReply header
  // - used e.g. by TMongoReplyCursor and TMongoConnection.GetReply()
  TMongoReplyHeader = packed record
    /// standard message header
    Header: TMongoWireHeader;
    /// response flags
    ResponseFlags: integer;
    /// cursor identifier if the client may need to perform further opGetMore
    CursorID: Int64;
    /// where in the cursor this reply is starting
    StartingFrom: integer;
    /// number of documents in the reply
    NumberReturned: integer;
  end;

  /// points to an low-level binary structure mapping the TMongoReply header
  // - so that you can write e.g.
  // ! PMongoReplyHeader(aMongoReply)^.RequestID
  PMongoReplyHeader = ^TMongoReplyHeader;


  /// map a MongoDB server reply message as sent by the database
  // - in response to TMongoRequestQuery / TMongoRequestGetMore messages
  // - you can use the record's methods to retrieve information about a given
  // response, and navigate within all nested documents
  // - several TMongoReplyCursor instances may map the same TMongoReply content
  // - you can safely copy one TMongoReplyCursor instance to another
  {$ifdef USERECORDWITHMETHODS}TMongoReplyCursor = record
    {$else}TMongoReplyCursor = object {$endif}
  private
    fReply: TMongoReply;
    fRequestID: integer;
    fResponseTo: integer;
    fResponseFlags: TMongoReplyCursorFlags;
    fCursorID: Int64;
    fStartingFrom: integer;
    fNumberReturned: integer;
    fDocuments: TPointerDynArray;
    fCurrentPosition: integer;
    fFirstDocument,
    fCurrentDocument: PAnsiChar;
    fLatestDocIndex: integer;
    fLatestDocValue: variant;
    procedure ComputeDocumentsList;
    function GetOneDocument(index: integer): variant;
  public
    /// initialize the cursor with a supplied binary reply from the server
    // - will raise an EMongoException if the content is not valid
    // - will populate all record fields with the supplied data
    procedure Init(const ReplyMessage: TMongoReply);

    /// retrieve the next document in the list, as a TDocVariant instance
    // - return TRUE if the supplied document has been retrieved
    // - return FALSE if there is no more document to get - you can use the
    // Rewind method to restart from the first document
    // - could be used e.g. as:
    // ! var Reply: TMongoReply;
    // !     doc: variant;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   while Reply.Next(doc) do
    // !      writeln('Name: ',doc.Name,' FirstName: ',doc.FirstName);
    function Next(out doc: variant; option: TBSONDocArrayConversion=asDocVariantPerReference): boolean; overload;
    /// retrieve the next document in the list, as BSON content
    // - return TRUE if the supplied document has been retrieved - then doc
    // points to a "int32 e_list #0" BSON document
    // - return FALSE if there is no more document to get - you can use the
    // Rewind method to restart from the first document
    // - this method is almost immediate, since the BSON raw binary is returned
    // directly without any conversion
    // - could be used e.g. as:
    // ! var Reply: TMongoReply;
    // !     doc: PByte;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   while Reply.Next(doc) do
    // !     writeln(BSONToJSON(doc,0,modMongoShell)); // fast display
    function Next(out doc: PByte): boolean; overload;
    /// retrieve the next document in the list, as a BSON binary document
    // - return TRUE if the supplied document has been retrieved - then doc
    // points to a "int32 e_list #0" BSON document
    // - return FALSE if there is no more document to get - you can use the
    // Rewind method to restart from the first document
    // - this method is slightly slower than the one returning a PByte, since
    // it will allocate a memory buffer to store the TBSONDocument binary
    // - could be used e.g. as:
    // ! var Reply: TMongoReply;
    // !     doc: TBSONDocument;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   while Reply.Next(doc) do
    // !     writeln(BSONToJSON(doc,0,modMongoShell)); // fast display
    function Next(out BSON: TBSONDocument): boolean; overload;
    /// retrieve the next document in the list, as JSON content
    // - return TRUE if the supplied document has been retrieved
    // - return FALSE if there is no more document to get - you can use the
    // Rewind method to restart from the first document
    // - could be used e.g. as:
    // ! var Reply: TMongoReply;
    // !     json: RawUTF8;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   while Reply.Next(json,modMongoShell) do
    // !     writeln(json); // fast display
    function Next(out JSON: RawUTF8; Mode: TMongoJSONMode=modMongoStrict): boolean; overload;
    /// let Next() overloaded methods point to the first document of this message
    procedure Rewind;

    /// retrieve a given document as a TDocVariant instance
    // - this method won't use any cache (like Document[..] property), since
    // it should be used with a local variant on stack as cache:
    // ! var Reply: TMongoReply;
    // !     doc: variant;
    // !     i: integer;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   for i := 0 to Reply.DocumentCount-1 do begin
    // !      GmrfQueryFailureetDocument(i,doc);
    // !      writeln('Name: ',doc.Name,' FirstName: ',doc.FirstName);
    // !   end;
    procedure GetDocument(index: integer; var result: variant);
    /// return all documents content as a JSON array, or one JSON object
    // if there is only one document in this reply
    // - this method is very optimized and will convert the BSON binary content
    // directly into JSON
    procedure FetchAllToJSON(W: TTextWriter; Mode: TMongoJSONMode=modMongoStrict;
      WithHeader: boolean=false; MaxSize: Cardinal=0);
    /// return all documents content as a JSON array, or one JSON object
    // if there is only one document in this reply
    // - this method is very optimized and will convert the BSON binary content
    // directly into JSON
    function ToJSON(Mode: TMongoJSONMode=modMongoStrict; WithHeader: boolean=false;
      MaxSize: Cardinal=0): RawUTF8;
    /// append all documents content to a dynamic array of TDocVariant
    // - return the new size of the Dest[] array
    function AppendAllToDocVariantDynArray(var Dest: TVariantDynArray): integer;
    /// append all documents content to a TDocVariant array instance
    // - if the supplied instance if not already a TDocVariant of kind dvArray,
    // a new void instance will be created
    // - return the new size of the Dest array
    function AppendAllToDocVariant(var Dest: TDocVariantData): integer;
    /// append all documents content to a BSON binary stream
    // - Dest.Tag will be used to count the current item number in the resulting
    // BSON array
    procedure AppendAllToBSON(Dest: TBSONWriter);

    /// retrieve the context execution of this message
    property ResponseFlags: TMongoReplyCursorFlags read fResponseFlags;
    /// identifier of this message
    property RequestID: integer read fRequestID;
    /// retrieve the RequestID from the original request
    property ResponseTo: integer read fResponseTo;
    /// access to the low-level binary reply message
    property Reply: TMongoReply read fReply;
    /// cursor identifier if the client may need to perform further
    // TMongoRequestGetMore messages
    // - in the event that the result set of the query fits into one OP_REPLY
    // message, CursorID will be 0
    property CursorID: Int64 read fCursorID;
    /// where in the cursor this reply is starting
    property StartingFrom: integer read fStartingFrom;
    /// number of documents in the reply
    property DocumentCount: integer read fNumberReturned;
    /// points to the first document binary
    // - i.e. just after the Reply header
    property FirstDocument: PAnsiChar read fFirstDocument;
    /// direct access to the low-level BSON binary content of each document
    property DocumentBSON: TPointerDynArray read fDocuments;
    /// retrieve a given document as a TDocVariant instance
    // - could be used e.g. as:
    // ! var Reply: TMongoReply;
    // !     i: integer;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   for i := 0 to Reply.DocumentCount-1 do
    // !      writeln('Name: ',Reply.Document[i].Name,' FirstName: ',Reply.Document[i].FirstName);
    // - note that there is an internal cache for the latest retrieved document
    // by this property, so that you can call Reply.Document[i] several times
    // without any noticeable speed penalty
    property Document[index: integer]: variant read GetOneDocument;
    /// the current position of the Next() call, starting at 0
    property Position: integer read fCurrentPosition;
  end;

  /// event callback signature for iterative process of TMongoConnection
  TOnMongoConnectionReply = procedure(Request: TMongoRequest;
    const Reply: TMongoReplyCursor; var Opaque) of object;

{$M+}

  TMongoClient = class;
  TMongoDatabase = class;
  TMongoCollection = class;

  /// one TCP/IP connection to a MongoDB server
  // - all access will be protected by a mutex (critical section): it is thread
  // safe but you may use one TMongoClient per thread or a connection pool, for
  // better performance
  TMongoConnection = class
  protected
    fLock: TRTLCriticalSection;
    fLocked: cardinal;
    fClient: TMongoClient;
    fSocket: TCrtSocket;
    fServerAddress: RawUTF8;
    fServerPort: integer;
    procedure Lock;
    procedure UnLock;
    function Send(Request: TMongoRequest): boolean;
    function GetOpened: boolean;
    function GetLocked: boolean;
    // will call TMongoReplyCursor.FetchAllToJSON(TTextWriter(Opaque))
    procedure ReplyJSONStrict(Request: TMongoRequest; const Reply: TMongoReplyCursor; var Opaque);
    procedure ReplyJSONExtended(Request: TMongoRequest; const Reply: TMongoReplyCursor; var Opaque);
    procedure ReplyJSONNoMongo(Request: TMongoRequest; const Reply: TMongoReplyCursor; var Opaque);
    // will call TMongoReplyCursor.AppendAllToDocVariantDynArray(TVariantDynArray(Opaque))
    procedure ReplyDocVariant(Request: TMongoRequest; const Reply: TMongoReplyCursor; var Opaque);
    // will call TMongoReplyCursor.AppendAllToBSON(TBSONWrite(Opaque))
    procedure ReplyBSON(Request: TMongoRequest; const Reply: TMongoReplyCursor; var Opaque);
  public
    /// initialize the connection to the corresponding MongoDB server
    // - the server address is either a host name, or an IP address
    // - if no server address is specified, will try to connect to localhost
    // - this won't create the connection, until Open method is executed
    constructor Create(const aClient: TMongoClient; const aServerAddress: RawByteString;
      aServerPort: integer=MONGODB_DEFAULTPORT); reintroduce;
    /// release the connection, including the socket
    destructor Destroy; override;
    /// connect to the MongoDB server
    // - will raise an EMongoException on error
    procedure Open;
    /// disconnect from MongoDB server
    // - will raise an EMongoException on error
    procedure Close;

    /// low-level method to send a request to the server
    // - if Request is not either TMongoRequestQuery or TMongoRequestGetMore,
    // will raise an EMongoException
    // - then will return the reply message as sent back by the database,
    // ready to be accessed using a TMongoReplyCursor wrapper
    procedure GetReply(Request: TMongoRequest; out result: TMongoReply);
    /// low-level method to send a request to the server, and return a cursor
    // - if Request is not either TMongoRequestQuery or TMongoRequestGetMore,
    // will raise an EMongoException
    // - then will parse and return a cursor to the reply message as sent back
    // by the database, with logging if necessary
    // - raise an EMongoException if mrfQueryFailure flag is set in the reply
    procedure GetCursor(Request: TMongoRequest; var Result: TMongoReplyCursor);
    /// low-level method to send a query to the server, calling a callback event
    // on each reply
    // - is used by GetDocumentsAndFree, GetBSONAndFree and GetJSONAndFree
    // methods to receive the whole document (you should better call those)
    // - the supplied Query instance will be released when not needed any more
    procedure GetRepliesAndFree(Query: TMongoRequestQuery;
      OnEachReply: TOnMongoConnectionReply; var Opaque);

    /// send a query to the server, returning a TDocVariant instance containing
    // all the incoming data
    // - will send the Request message, and any needed TMongoRequestGetMore
    // messages to retrieve all the data from the server
    // - the supplied Query instance will be released when not needed any more
    // - if Query.NumberToReturn<>1, it will return either null or a dvArray
    // kind of TDocVariant containing all returned items
    // - if Query.NumberToReturn=1, then it will return either null or a
    // single TDocVariant instance
    function GetDocumentsAndFree(Query: TMongoRequestQuery): variant; overload;
    /// send a query to the server, returning a TDocVariant instance containing
    // all the incoming data
    // - will send the Request message, and any needed TMongoRequestGetMore
    // messages to retrieve all the data from the server
    // - the supplied Query instance will be released when not needed any more
    // - if Query.NumberToReturn<>1, it will return either null or a dvArray
    // kind of TDocVariant containing all returned items
    // - if Query.NumberToReturn=1, then it will return either null or a
    // single TDocVariant instance
    procedure GetDocumentsAndFree(Query: TMongoRequestQuery; var result: variant); overload;
    /// send a query to the server, returning a dynamic array of TDocVariant
    // instance containing all the incoming data
    // - will send the Request message, and any needed TMongoRequestGetMore
    // messages to retrieve all the data from the server
    // - the supplied Query instance will be released when not needed any more
    procedure GetDocumentsAndFree(Query: TMongoRequestQuery; var result: TVariantDynArray); overload;
    /// send a query to the server, returning a TBSONDocument instance containing
    // all the incoming data, as raw binary BSON document containing an array
    // of the returned items
    // - will send the Request message, and any needed TMongoRequestGetMore
    // messages to retrieve all the data from the server
    // - the supplied Query instance will be released when not needed any more
    function GetBSONAndFree(Query: TMongoRequestQuery): TBSONDocument;
    /// send a query to the server, returning all the incoming data as JSON
    // - will send the Request message, and any needed TMongoRequestGetMore
    // messages to retrieve all the data from the server
    // - this method is very optimized and will convert the BSON binary content
    // directly into JSON, in either modMongoStrict or modMongoShell layout
    // (modNoMongo will do the same as modMongoStrict)
    // - if Query.NumberToReturn<>1, it will return either 'null' or a '[..]'
    // JSON array with all the incoming documents retrieved from the server
    // - if Query.NumberToReturn=1, it will return either 'null' or a single
    // '{...}' JSON object
    // - the supplied Query instance will be released when not needed any more
    function GetJSONAndFree(Query: TMongoRequestQuery; Mode: TMongoJSONMode): RawUTF8;

    /// send a message to the MongoDB server
    // - will apply Client.WriteConcern policy, and run an EMongoException
    // in case of any error
    // - the supplied Request instance will be released when not needed any more
    // - by default, it will follow Client.WriteConcern pattern - but you can
    // set NoAcknowledge = TRUE to avoid calling the getLastError command
    // - will return the getLastError reply (if retrieved from server)
    function SendAndFree(Request: TMongoRequest; NoAcknowledge: boolean=false): variant;
    /// run a database command, supplied as a TDocVariant, TBSONVariant or a
    // string, and return the a TDocVariant instance
    // - see http://docs.mongodb.org/manual/reference/command for a list
    // of all available commands
    // - for instance:
    // ! RunCommand('test',_ObjFast(['dbStats',1,'scale',1024],stats);
    // ! RunCommand('test',BSONVariant(['dbStats',1,'scale',1024],stats);
    // ! RunCommand('admin','buildinfo',fServerBuildInfo);
    // - the message will be returned by the server as a single TDocVariant
    // instance (since the associated TMongoRequestQuery.NumberToSkip=1)
    // - in case of any error, the error message is returned as text
    // - in case of success, this method will return ''
    function RunCommand(const aDatabaseName: RawUTF8;
      const command: variant; var returnedValue: variant;
      flags: TMongoQueryFlags=[]): RawUTF8; overload;
    /// run a database command, supplied as a TDocVariant, TBSONVariant or a
    // string, and return the raw BSON document array of received items
    // - this overloaded method can be used on huge content to avoid the slower
    // conversion to an array of TDocVariant instances
    // - in case of success, this method will return TRUE, or FALSE on error
    function RunCommand(const aDatabaseName: RawUTF8;
      const command: variant; var returnedValue: TBSONDocument;
      flags: TMongoQueryFlags=[]): boolean; overload;

    /// return TRUE if the Open method has successfully been called
    property Opened: boolean read GetOpened;
    /// access to the corresponding MongoDB server
    property Client: TMongoClient read fClient;
    /// direct access to the low-level TCP/IP communication socket
    property Socket: TCrtSocket read fSocket;
    /// is TRUE when the connection is busy
    property Locked: boolean read GetLocked;
  published
    /// read-only access to the supplied server address
    // - the server address is either a host name, or an IP address
    property ServerAddress: RawUTF8 read fServerAddress;
    /// read-only access to the supplied server port
    // - the server Port is MONGODB_DEFAULTPORT (27017) by default
    property ServerPort: integer read fServerPort;
  end;

  /// array of TCP connection to a MongoDB Replica Set
  // - first item [0] is the Primary member
  // - other items [1..] are the Secondary members
  TMongoConnectionDynArray = array of TMongoConnection;

  /// define Read Preference Modes to a MongoDB replica set
  // - Important: All read preference modes except rpPrimary may return stale
  // data because secondaries replicate operations from the primary with some
  // delay - ensure that your application can tolerate stale data if you choose
  // to use a non-primary mode
  // - rpPrimary: Default mode - all operations read from the current replica
  // set primary
  // - rpPrimaryPreferred: in most situations, operations read from the primary
  // but if it is unavailable, operations read from secondary members.
  // - rpPsecondary: all operations read from the secondary members
  // of the replica set
  // - rpPsecondaryPreferred: in most situations, operations read from
  // secondary members but if no secondary members are available, operations
  // read from the primary
  // rpNearest: read from the member of the replica set with the least network
  // latency, irrespective of whether that member is a primary or secondary
  // (in practice, we won't use latency, just a random distribution)
  TMongoClientReplicaSetReadPreference = (
    rpPrimary, rpPrimaryPreferred, rpSecondary, rpSecondaryPreferred, rpNearest);

  /// define Write Concern property of a MongoDB connection
  // - Write concern describes the guarantee that MongoDB provides when
  // reporting on the success of a write operation. The strength of the write
  // concerns determine the level of guarantee. When inserts, updates and
  // deletes have a weak write concern, write operations return quickly. In
  // some failure cases, write operations issued with weak write concerns may
  // not persist. With stronger write concerns, clients wait after sending a
  // write operation for MongoDB to confirm the write operations. MongoDB
  // provides different levels of write concern to better address the specific
  // needs of applications. Clients may adjust write concern to ensure that
  // the most important operations persist successfully to an entire
  // MongoDB deployment. For other less critical operations, clients can
  // adjust the write concern to ensure faster performance rather than
  // ensure persistence to the entire deployment.
  // - wcAcknowledged is the default safe mode: the mongod confirms the
  // receipt of the write operation. Acknowledged write concern allows clients
  // to catch network, duplicate key, and other errors.
  // - with wcJournaled, the mongod acknowledges the write operation only
  // after committing the data to the journal. This write concern ensures that
  // MongoDB can recover the data following a shutdown or power interruption.
  // - wcReplicaAcknowledged will guarantee that the write operation propagates
  // to at least one member of a replica set
  // - with wcUnacknowledged, MongoDB does not acknowledge the receipt of
  // write operation. Unacknowledged is similar to errors ignored; however,
  // drivers attempt to receive and handle network errors when possible. The
  // driver's ability to detect network errors depends on the system's
  // networking configuration.
  // - with wcErrorsIgnored, MongoDB does not acknowledge write operations.
  // With this level of write concern, the client cannot detect failed write
  // operations. These errors include connection errors and mongod exceptions
  // such as duplicate key exceptions for unique indexes. Although the errors
  // ignored write concern provides fast performance, this performance gain
  // comes at the cost of significant risks for data persistence and durability.
  // WARNING: Do not use wcErrorsIgnored write concern in normal operation.
  TMongoClientWriteConcern = (
    wcAcknowledged, wcJournaled, wcReplicaAcknowledged,
    wcUnacknowledged, wcErrorsIgnored);

  /// remote access to a MongoDB server
  // - a single server can have several active connections, if some secondary
  // hosts were defined
  TMongoClient = class
  protected
    fConnectionString: RawUTF8;
    fDatabases: TRawUTF8List;
    fConnections: TMongoConnectionDynArray;
    fReadPreference: TMongoClientReplicaSetReadPreference;
    fWriteConcern: TMongoClientWriteConcern;
    fConnectionTimeOut: Cardinal;
    fConnectionTLS: boolean;
    fGracefulReconnect: record
      Enabled, ForcedDBCR: boolean;
      User, Database: RawUTF8;
      EncryptedDigest: RawByteString;
    end;
    fLog: TSynLog;
    fLogRequestEvent: TSynLogInfo;
    fLogReplyEvent: TSynLogInfo;
    fLogReplyEventMaxSize: cardinal;
    fServerBuildInfo: variant;
    fServerBuildInfoNumber: cardinal;
    fLatestReadConnectionIndex: PtrInt;
    procedure AfterOpen(ConnectionIndex: PtrInt); virtual;
    function GetBytesReceived: Int64;
    function GetBytesSent: Int64;
    function GetBytesTransmitted: Int64;
    procedure Auth(const DatabaseName,UserName,Digest: RawUTF8;
      ForceMongoDBCR: boolean; ConnectionIndex: PtrInt);
    function ReOpen: boolean;
  public
    /// prepare a connection to a MongoDB server or Replica Set
    // - this constructor won't create the connection until the Open method
    // is called
    // - you can specify multiple hosts, as CSV values, if necessary
    // - depending on the platform, you may request for a TLS secured connection
    constructor Create(const Host: RawUTF8; Port: integer=MONGODB_DEFAULTPORT;
      aTLS: boolean=false; const SecondaryHostCSV: RawUTF8=''; const SecondaryPortCSV: RawUTF8=''); overload;
    /// connect to a database on a remote MongoDB primary server
    // - this method won't use authentication, and will return the corresponding
    // MongoDB database instance
    // - this method is an alias to the Database[] property
    function Open(const DatabaseName: RawUTF8): TMongoDatabase;
    /// secure connection to a database on a remote MongoDB server
    // - this method will use authentication and will return the corresponding
    // MongoDB database instance, with a dedicated secured connection
    // - will use MONGODB-CR for MongoDB engines up to 2.6 (or if ForceMongoDBCR
    // is TRUE), and SCRAM-SHA-1 since MongoDB 3.x
    // - see http://docs.mongodb.org/manual/administration/security-access-control
    function OpenAuth(const DatabaseName,UserName,PassWord: RawUTF8;
      ForceMongoDBCR: boolean=false): TMongoDatabase;
    /// close the connection and release all associated TMongoDatabase,
    // TMongoCollection and TMongoConnection instances
    destructor Destroy; override;
    /// define an optional logging instance to be used
    // - you can also specify the event types to be used for requests or
    // replay: by default, a verbose log with sllSQL and sllDB will be set
    // - e.g. mORMotMongoDB.pas will call Client.SetLog(SQLite3Log) for you
    procedure SetLog(LogClass: TSynLogClass;
      RequestEvent: TSynLogInfo=sllSQL; ReplyEvent: TSynLogInfo=sllDB;
      ReplyEventMaxSize: cardinal=1024);

    /// retrieve extended server version and build information, as text
    // - will create a string from ServerBuildInfo object, e.g. as
    // $ 'MongoDB 3.2.0 mozjs mmapv1,wiredTiger'
    function ServerBuildInfoText: RawUTF8;
    // select Connection in dependence of ReadPreference
    function GetOneReadConnection: TMongoConnection;
    /// retrieve the server version and build information
    // - return the content as a TDocVariant document, e.g.
    // ! ServerBuildInfo.version = '2.4.9'
    // ! ServerBuildInfo.versionArray = [2,4,9,0]
    // - this property is cached, so request is sent only once
    // - you may rather use ServerBuildInfoNumber to check for available
    // features at runtime, for easy comparison of the server version
    property ServerBuildInfo: variant read fServerBuildInfo;
    /// access to a given MongoDB database
    // - try to open it via a non-authenticated connection it if not already:
    // will raise an exception on error, or will return an instance
    // - will return an existing instance if has already been opened
    property Database[const DatabaseName: RawUTF8]: TMongoDatabase read Open; default;
    /// low-level access to the TCP/IP connections of this MongoDB replica set
    // - first item [0] is the Primary member
    // - other items [1..] are the Secondary members
    property Connections: TMongoConnectionDynArray read fConnections;
    /// define the logging instance to be used for LogRequestEvent/LogReplyEvent
    // - you may also call the SetLog() method to set all options at once
    property Log: TSynLog read fLog write fLog;
  published
    /// the connection definition used to connect to this MongoDB server
    property ConnectionString: RawUTF8 read fConnectionString;
    /// retrieve the server version and build information
    // - return the content as a TDocVariant document, e.g.
    // ! 2040900 for MongoDB 2.4.9, or 2060000 for MongoDB 2.6, or
    // ! 3000300 for MongoDB 3.0.3
    // - this property is cached, so can be used to check for available
    // features at runtime, without any performance penalty
    property ServerBuildInfoNumber: cardinal read fServerBuildInfoNumber;
    /// define Read Preference mode to a MongoDB replica set
    // - see http://docs.mongodb.org/manual/core/read-preference
    // - default is rpPrimary, i.e. reading from the main primary instance
    // - Important: All read preference modes except rpPrimary may return stale
    // data because secondaries replicate operations from the primary with some
    // delay - ensure that your application can tolerate stale data if you choose
    // to use a non-primary mode
    property ReadPreference: TMongoClientReplicaSetReadPreference
      read fReadPreference write fReadPreference;
    /// define Write Concern mode to a MongoDB replica set
    // - see http://docs.mongodb.org/manual/core/write-concern
    // - default is wcAcknowledged, i.e. to acknowledge all write operations
    property WriteConcern: TMongoClientWriteConcern
      read fWriteConcern write fWriteConcern;
    /// the connection time out, in milliseconds
    // - default value is 30000, i.e. 30 seconds
    property ConnectionTimeOut: Cardinal read fConnectionTimeOut write fConnectionTimeOut;
    /// if the socket connection is secured over TLS
    property ConnectionTLS: boolean read fConnectionTLS;
    /// allow automatic reconnection (with authentication, if applying), if the
    // socket is closed (e.g. was dropped from the server)
    property GracefulReconnect: boolean
      read fGracefulReconnect.Enabled write fGracefulReconnect.Enabled;
    /// how may bytes this client did received, among all its connections
    property BytesReceived: Int64 read GetBytesReceived;
    /// how may bytes this client did received, among all its connections
    property BytesSent: Int64 read GetBytesSent;
    /// how may bytes this client did transmit, adding both input and output
    property BytesTransmitted: Int64 read GetBytesTransmitted;
    /// if set to something else than default sllNone, will log each request
    // with the corresponding logging event kind
    // - will use the Log property for the destination log
    // - you may also call the SetLog() method to set all options at once
    property LogRequestEvent: TSynLogInfo read fLogRequestEvent write fLogRequestEvent;
    /// if set to something else than default sllNone, will log each reply
    // with the corresponding logging event kind
    // - WARNING: logging all incoming data may be very verbose, e.g. when
    // retrieving a document list - use it with care, not on production, but
    // only for debugging purposes - or set LogReplyEventMaxSize to a low value
    // - will use the Log property for the destination log
    // - you may also call the SetLog() method to set all options at once
    property LogReplyEvent: TSynLogInfo read fLogReplyEvent write fLogReplyEvent;
    /// defines how many characters a LogReplyEvent entry may append in the log
    // - is set by default to 1024, which sounds somewhat good for debugging
    property LogReplyEventMaxSize: cardinal
      read fLogReplyEventMaxSize write fLogReplyEventMaxSize;
  end;

  /// remote access to a MongoDB database
  TMongoDatabase = class
  protected
    fClient: TMongoClient;
    fName: RawUTF8;
    fCollections: TRawUTF8List;
    function GetCollection(const Name: RawUTF8): TMongoCollection;
    function GetCollectionOrCreate(const Name: RawUTF8): TMongoCollection;
    function GetCollectionOrNil(const Name: RawUTF8): TMongoCollection;
  public
    /// initialize a reference to a given MongoDB Database
    // - you should not use this constructor directly, but rather use the
    // TMongoClient.Database[] property
    // - it will connect to the Client's primary host, then retrieve all
    // collection names of this database
    constructor Create(aClient: TMongoClient; const aDatabaseName: RawUTF8);
    /// release all associated TMongoCollection instances
    destructor Destroy; override;

    /// run a database command, supplied as a TDocVariant, TBSONVariant or a
    // string, and return a TDocVariant instance
    // - this is the preferred method to issue database commands, as it provides
    // a consistent interface between the MongoDB shell and this driver
    // - see http://docs.mongodb.org/manual/reference/command for a list
    // of all available commands
    // - for instance:
    // ! RunCommand(_ObjFast(['dbStats',1,'scale',1024],stats);
    // ! RunCommand(BSONVariant(['dbStats',1,'scale',1024],stats);
    // ! RunCommand('dbStats',stats);
    // ! RunCommand('hostInfo',host);
    // - the message will be returned by the server as a TDocVariant instance
    // (since the associated TMongoRequestQuery.NumberToSkip=1)
    // - in case of any error, the error message is returned as text
    // - in case of success, this method will return ''
    function RunCommand(const command: variant;
      var returnedValue: variant): RawUTF8; overload;
    /// run a database command, supplied as a TDocVariant, TBSONVariant or a
    // string, and return the raw BSON document array of received items
    // - this overloaded method can be used on huge content to avoid the slower
    // conversion to an array of TDocVariant instances
    // - in case of success, this method will return TRUE, or FALSE on error
    function RunCommand(const command: variant;
      var returnedValue: TBSONDocument): boolean; overload;

    /// create the user in the database to which the user will belong
    // - you could specify the roles to use, for this database or others:
    // ! reportingDB.CreateUser('reportsUser','12345678',BSONVariant(
    // !  '[{ role: "readWrite", db: "reporting" }, { role: "read", db: "products" }]'));
    // - returns '' on sucess, an error message otherwise
    function CreateUser(const UserName,Password: RawUTF8;
      const roles: variant): RawUTF8;
    /// create the user with a read or read/write role on the current database
    // - returns '' on sucess, an error message otherwise
    function CreateUserForThisDatabase(const UserName,Password: RawUTF8;
      allowWrite: Boolean=true): RawUTF8;
    /// deletes the supplied user on the current database
    // - returns '' on sucess, an error message otherwise
    function DropUser(const UserName: RawUTF8): RawUTF8;

    /// access to a given MongoDB collection
    // - raise an EMongoDatabaseException if the collection name does not exist
    property Collection[const Name: RawUTF8]: TMongoCollection
      read GetCollection; default;
    /// access to a given MongoDB collection
    // - if the collection name does not exist, it will return nil
    property CollectionOrNil[const Name: RawUTF8]: TMongoCollection
      read GetCollectionOrNil;
    /// access to a given MongoDB collection
    // - if the collection name does not exist, it will add use the name to
    // create a TMongoCollection instance and register it to the internal list
    property CollectionOrCreate[const Name: RawUTF8]: TMongoCollection
      read GetCollectionOrCreate;
  published
    /// the database name
    property Name: RawUTF8 read fName;
    /// the associated MongoDB client instance
    property Client: TMongoClient read fClient;
  end;

  /// remote access to a MongoDB collection
  TMongoCollection = class
  protected
    fDatabase: TMongoDatabase;
    fName: RawUTF8;
    fFullCollectionName: RawUTF8;
    function AggregateCallFromJSON(const pipelineJSON: RawUTF8; var reply,res: variant): boolean; overload;
    function AggregateCallFromVariant(const pipelineArray: variant; var reply,res: variant): boolean; overload;
  public
    /// initialize a reference to a given MongoDB Collection
    // - you should not use this constructor directly, but rather use
    // TMongoClient.Database[].Collection[] property
    constructor Create(aDatabase: TMongoDatabase; const aCollectionName: RawUTF8);

    /// select documents in a collection and returns a dvArray TDocVariant
    // instance containing the selected documents
    // - Criteria can be null (to retrieve all documents) or a TDocVariant /
    // TBSONVariant query selector:
    // ! FindDoc(BSONVariant('{name:"John",age:{$gt:21}}'),null);
    // ! FindDoc(BSONVariant('{name:?,age:{$gt:?}}',[],['John',21]),null);
    // see http://docs.mongodb.org/manual/reference/operator for reference
    // - Projection can be null (to retrieve all fields) or a CSV string to set
    // field names to retrieve, or a TDocVariant or TBSONVariant - e.g.:
    // ! FindDoc(BSONVariant(['name','John']),null);
    // ! FindDoc(BSONVariant(['name','John']),'_id,name');
    // ! FindDoc(BSONVariant(['name','John']),BSONVariantFieldSelector('name,_id'));
    // - NumberToReturn can be left to its default maxInt value to return all
    // matching documents, or specify a limit (e.g. 1 for one document - in this
    // case, the returned instance won't be a dvArray kind of TDocVariant, but
    // either null or the single returned document)
    // - if the query does not have any matching record, it will return null
    function FindDoc(const Criteria, Projection: Variant;
      NumberToReturn: integer=1; NumberToSkip: integer=0;
      Flags: TMongoQueryFlags=[]): variant; overload;
    /// select documents in a collection and returns a dvArray TDocVariant
    // instance containing the selected documents
    // - Criteria can specify the query selector as (extended) JSON and
    // parameters:
    // ! FindDoc('{name:"John",age:{$gt:21}}',[]);
    // ! FindDoc('{name:?,age:{$gt:?}}',['John',21]);
    // see http://docs.mongodb.org/manual/reference/operator for reference
    // - this overloaded method will use a null Projection, i.e. will retrieve
    // all fields
    // - NumberToReturn can be left to its default maxInt value to return all
    // matching documents, or specify a limit (e.g. 1 for one document - in this
    // case, the returned instance won't be a dvArray kind of TDocVariant, but
    // either null or the single returned document)
    // - if the query does not have any matching record, it will return null
    function FindDoc(Criteria: PUTF8Char; const Params: array of const;
      NumberToReturn: integer=maxInt; NumberToSkip: integer=0;
      Flags: TMongoQueryFlags=[]): variant; overload;
    /// find an existing document in a collection, by its _id field
    // - _id will identify the unique document to be retrieved
    // - returns null, or a TDocVariant instance
    function FindOne(const _id: TBSONObjectID): variant; overload;
    /// find an existing document in a collection, by its _id field
    // - _id will identify the unique document to be retrieved
    // - returns null, or a TDocVariant instance
    function FindOne(const _id: variant): variant; overload;
    /// find an existing document in a collection, by a custom Criteria value
    // - Criteria object, specified as name/value pairs, will identify the
    // unique document to be retrieved
    // - returns the found TDocVariant instance
    // - if the Criteria has no match, return either null or a new object with
    // default values as NameValuePairs if ReturnNewObjectIfNotFound is true
    function FindOne(const NameValuePairs: array of const;
      ReturnNewObjectIfNotFound: boolean=false): variant; overload;
    /// returns a dynamic array of TDocVariant instance containing
    // all documents of a collection
    // - Projection can be null (to retrieve all fields) or a CSV string to set
    // field names to retrieve, or a TDocVariant or TBSONVariant with
    // projection operators
    procedure FindDocs(var result: TVariantDynArray; const Projection: variant;
      NumberToReturn: integer=maxInt; NumberToSkip: integer=0;
      Flags: TMongoQueryFlags=[]); overload;
    /// select documents in a collection and returns a dynamic array of
    // TDocVariant instance containing the selected documents
    // - you can e.g. fill a res: TVariantDynArray with the following query:
    // ! FindDocs('{name:?,age:{$gt:?}}',['John',21],res,null);
    // - Projection can be null (to retrieve all fields) or a CSV string to set
    // field names to retrieve, or a TDocVariant or TBSONVariant with
    // projection operators
    procedure FindDocs(Criteria: PUTF8Char; const Params: array of const;
      var result: TVariantDynArray; const Projection: variant;
      NumberToReturn: integer=maxInt; NumberToSkip: integer=0;
      Flags: TMongoQueryFlags=[]); overload;
    /// select documents in a collection and returns a dynamic array of
    // TDocVariant instance containing the selected documents
    // - could be used to fill a VCL grid using a TDocVariantArrayDataSet
    // as defined in SynVirtualDataSet.pas:
    // ! ds1.DataSet := ToDataSet(self,FindDocs('{name:?,age:{$gt:?}}',['John',21],null));
    // - Projection can be null (to retrieve all fields) or a CSV string to set
    // field names to retrieve, or a TDocVariant or TBSONVariant with
    // projection operators
    function FindDocs(Criteria: PUTF8Char; const Params: array of const;
      const Projection: variant; NumberToReturn: integer=maxInt; NumberToSkip: integer=0;
      Flags: TMongoQueryFlags=[]): TVariantDynArray; overload;

    /// select documents in a collection and returns a JSON array of documents
    // containing the selected documents
    // - Criteria can be null (to retrieve all documents) or a TDocVariant /
    // TBSONVariant query selector:
    // ! FindJSON(BSONVariant('{name:"John",age:{$gt:21}}'),null);
    // ! FindJSON(BSONVariant('{name:?,age:{$gt:?}}',[],['John',21]),null);
    // see http://docs.mongodb.org/manual/reference/operator for reference
    // - Projection can be null (to retrieve all fields) or a CSV string to set
    // the field names to retrieve, or a TDocVariant or TBSONVariant - e.g.:
    // ! FindJSON(BSONVariant(['name','John']),null);
    // ! FindJSON(BSONVariant(['name','John']),'_id');
    // ! FindJSON(BSONVariant(['name','John']),BSONVariantFieldSelector('name,_id'));
    // - NumberToReturn can be left to its default maxInt value to return all
    // matching documents as a '[..]' JSON array, or specify a limit (e.g. 1
    // for one document - in this case, the returned instance won't be a '[..]'
    // JSON array, but either 'null' or a single '{..}' JSON object)
    // - this method is very optimized and will convert the BSON binary content
    // directly into JSON, in either modMongoStrict or modMongoShell layout
    // (modNoMongo will do the same as modMongoStrict)
    function FindJSON(const Criteria, Projection: Variant;
      NumberToReturn: integer=maxInt; NumberToSkip: integer=0;
      Flags: TMongoQueryFlags=[]; Mode: TMongoJSONMode=modMongoStrict): RawUTF8; overload;
    /// select documents in a collection and returns a JSON array of documents
    // containing the selected documents
    // - Criteria can specify the query selector as (extended) JSON and
    // parameters:
    // ! FindJSON('{name:"John",age:{$gt:21}}',[]);
    // ! FindJSON('{name:?,age:{$gt:?}}',['John',21]);
    // see http://docs.mongodb.org/manual/reference/operator for reference
    // - this overloaded method will use a null Projection, i.e. will retrieve
    // all fields
    // - NumberToReturn can be left to its default maxInt value to return all
    // matching documents as a '[..]' JSON array, or specify a limit (e.g. 1
    // for one document - in this case, the returned instance won't be a '[..]'
    // JSON array, but either 'null' or a single '{..}' JSON object)
    function FindJSON(Criteria: PUTF8Char; const Params: array of const;
      NumberToReturn: integer=maxInt; NumberToSkip: integer=0;
      Flags: TMongoQueryFlags=[]; Mode: TMongoJSONMode=modMongoStrict): RawUTF8; overload;
    /// select documents in a collection and returns a JSON array of documents
    // containing the selected documents
    // - Criteria and Projection can specify the query selector as (extended)
    // JSON and parameters
    function FindJSON(Criteria: PUTF8Char; const CriteriaParams: array of const;
      const Projection: variant; NumberToReturn: integer=maxInt; NumberToSkip: integer=0;
      Flags: TMongoQueryFlags=[]; Mode: TMongoJSONMode=modMongoStrict): RawUTF8; overload;

    /// select documents in a collection and returns a TBSONDocument instance
    // containing the selected documents as a raw binary BSON array document
    // - Criteria can be null (to retrieve all documents) or a TDocVariant /
    // TBSONVariant query selector:
    // ! FindBSON(BSONVariant('{name:"John",age:{$gt:21}}'),null);
    // ! FindBSON(BSONVariant('{name:?,age:{$gt:?}}',[],['John',21]),null);
    // - Projection can be null (to retrieve all fields) or a CSV string to set
    // the field names to retrieve, or a TDocVariant or TBSONVariant - e.g.:
    // ! FindBSON(BSONVariant(['name','John']),null);
    // ! FindBSON(BSONVariant(['name','John']),'_id');
    // ! FindBSON(BSONVariant(['name','John']),BSONVariantFieldSelector('name,_id'));
    // - NumberToReturn can be left to its default maxInt value to return all
    // matching documents, or specify a limit (e.g. 1 for one document)
    function FindBSON(const Criteria, Projection: Variant;
      NumberToReturn: integer=maxInt; NumberToSkip: integer=0;
      Flags: TMongoQueryFlags=[]): TBSONDocument;

    /// insert one document, supplied as (extended) JSON and parameters,
    // in the collection
    // - supplied JSON could be either strict or in MongoDB Shell syntax:
    // !   products.insert('{ _id: ?, item: ?, qty: ? }',[1,'card',15]);
    // !   // here _id is forced on the client side
    // !   products.insert('{ item: ?, qty: ? }',[1,'card',15]);
    // !   // here the _id will be created on the client side as an ObjectID
    // - you can retrieve the associated ObjectID, as such:
    // ! var oid: TBSONObjectID;
    // ! ...
    // !   products.insert('{ item: ?, qty: ? }',['card',15],@oid);
    // !   writeln(oid.ToText);
    procedure Insert(const Document: RawUTF8; const Params: array of const;
      DocumentObjectID: PBSONObjectID=nil); overload;
    /// insert one or more documents in the collection
    // - Documents is an array of TDocVariant (i.e. created via _JsonFast()
    // or _JsonFastFmt()) - or of TBSONVariant (created via BSONVariant())
    // - by default, it will follow Client.WriteConcern pattern - but you can
    // set NoAcknowledge = TRUE to avoid calling the getLastError command and
    // increase the execution speed, at the expense of a unsafe process
    procedure Insert(const Documents: array of variant; Flags: TMongoInsertFlags=[];
      NoAcknowledge: boolean=false); overload;
    /// insert one or more documents in the collection
    // - Documents is the low-level concatenation of BSON documents, created
    // e.g. with a TBSONWriter stream
    // - by default, it will follow Client.WriteConcern pattern - but you can
    // set NoAcknowledge = TRUE to avoid calling the getLastError command and
    // increase the execution speed, at the expense of a unsafe process
    procedure Insert(const Documents: TBSONDocument;
      Flags: TMongoInsertFlags=[]; NoAcknowledge: boolean=false); overload;
    /// insert one or more documents in the collection
    // - JSONDocuments is an array of JSON objects
    // - by default, it will follow Client.WriteConcern pattern - but you can
    // set NoAcknowledge = TRUE to avoid calling the getLastError command and
    // increase the execution speed, at the expense of a unsafe process
    procedure InsertJSON(const JSONDocuments: array of PUTF8Char;
      Flags: TMongoInsertFlags=[]; NoAcknowledge: boolean=false);

    /// updates an existing document or inserts a new document, depending on
    // its document parameter
    // - this document should be a TDocVariant (i.e. created via _JsonFast()
    // or _JsonFastFmt()) since we need to check for the _id field, other types
    // will be converted to a TDocVariant instance (via its JSON representation)
    // so it is pointless to use BSONVariant() here
    // - if the document does not contain an _id field, then the Save() method
    // performs an insert; during the operation, the client will add to the
    // Document variant the _id field and assign it a unique ObjectId - and the
    // method returns FALSE
    // - if the document contains an _id field, then the save() method performs
    // an upsert, querying the collection on the _id field: if a document does
    // not exist with the specified _id value, the save() method performs an
    // insert; if a document exists with the specified _id value, the save()
    // method performs an update that replaces ALL fields in the existing
    // document with the fields from the document - and the method returns TRUE
    // - you can optionally retrieve the _id value with the DocumentObjectID pointer
    function Save(var Document: variant; DocumentObjectID: PBSONObjectID=nil): boolean; overload;
    /// updates an existing document or inserts a new document, depending on
    // its document parameter, supplied as (extended) JSON and parameters
    // - supplied JSON could be either strict or in MongoDB Shell syntax:
    // - will perform either an insert or an update, depending of the
    // presence of the _id field, as overloaded Save(const Document: variant)
    procedure Save(const Document: RawUTF8; const Params: array of const;
      DocumentObjectID: PBSONObjectID=nil); overload;

    /// modifies an existing document or several documents in a collection
    // - the method can modify specific fields of existing document or documents
    // or replace an existing document entirely, depending on the update parameter
    // - Query and Update parameters should be TDocVariant (i.e. created via
    // _JsonFast() or _JsonFastFmt()) or TBSONVariant (created via BSONVariant())
    // - Query is the selection criteria for the update; use the same query
    // selectors as used in the Find() method
    // - if Update contains a plain document, it will replace any existing data
    // - if Update contains update operators (like $set), it will update the
    // corresponding fields in the document
    procedure Update(const Query, Update: variant; Flags: TMongoUpdateFlags=[]); overload;
    /// modifies an existing document or several documents in a collection
    // - the method can modify specific fields of existing document or documents
    // or replace an existing document entirely, depending on the update parameter
    // - since all content will be transformed into JSON internally, use this
    // method only if the supplied parameters are simple types: any complex value
    // (e.g. a TDateTime or a BSONVariant binary) won't be handled as expected -
    // use the overloaded Update() with explicit BSONVariant() values instead
    // - Query and Update parameters can be specified as JSON objects with
    // parameters
    // - Query is the selection criteria for the update; use the same query
    // selectors as used in the Find() method
    // - if Update contains a plain document, it will replace any existing data:
    // ! people.update('{name:?}',['Andy'],'{name:?,age:? }',['Andy',25],[mufUpsert]);
    // Warning: to avoid inserting the same document more than once, only use
    // mufUpsert if the query field is uniquely indexed
    // - if Update contains update operators (like $set), it will update the
    // corresponding fields in the document:
    // ! book.insert('{_id:?,item:?,stock:?}',[11,'Divine Comedy',2]);
    // ! book.update('{item:?},['Divine Comedy'],'{$set:{price:?},$inc:{stock:?}},[18,5]);
    // ! // the updated document is now:
    // ! { "_id" : 11, "item" : "Divine Comedy", "price" : 18, "stock" : 7 }
    procedure Update(Query: PUTF8Char; const QueryParams: array of const;
      const Update: RawUTF8; const UpdateParams: array of const;
      Flags: TMongoUpdateFlags=[]); overload;
    /// modifies some fields of an existing document in a collection
    // - by default, Update() or Save() will replace the whole document
    // - this method will expect the identifier to be supplied as a variant -
    // may be via the ObjectID() function
    // - and will replace the specified fields, i.e. it will execute a $set:
    // with the supplied UpdatedFields value
    procedure UpdateOne(const _id, UpdatedFields: variant);

    /// delete an existing document or several documents in a collection
    // - Query parameter should be TDocVariant (i.e. created via _JsonFast() or
    // _JsonFastFmt()) or TBSONVariant (created via BSONVariant())
    // - Query is the selection criteria for the deletion; use the same query
    // selectors as used in the Find() method
    // - to limit the deletion to just one document, set Flags to [mdfSingleRemove]
    // - to delete all documents matching the deletion criteria, leave it to []
    procedure Remove(const Query: variant; Flags: TMongoDeleteFlags=[]); overload;
    /// delete an existing document in a collection, by its _id field
    // - _id will identify the unique document to be deleted
    procedure RemoveOne(const _id: TBSONObjectID); overload;
    /// delete an existing document in a collection, by its _id field
    // - _id will identify the unique document to be deleted
    procedure RemoveOne(const _id: variant); overload;
    /// delete an existing document or several documents in a collection
    // - Query parameter can be specified as JSON objects with parameters
    // - Query is the selection criteria for the deletion; use the same query
    // selectors as used in the Find() method
    // - to limit the deletion to just one document, set Flags to [mdfSingleRemove]
    // - to delete all documents matching the deletion criteria, leave it to []
    procedure RemoveFmt(Query: PUTF8Char; const QueryParams: array of const;
       Flags: TMongoDeleteFlags=[]);

    /// creates an index on the specified field(s) if the index does
    // not already exist
    // - Keys and Options parameters should be TDocVariant (e.g. created via
    // _JsonFast() or _JsonFastFmt()) - and not TBSONVariant values
    // - for ascending/descending indexes, Keys is a document that contains pairs
    // with the name of the field or fields to index and order of the index:
    // value of 1 specifies ascending and of -1 specifies descending
    // - options is a non-mandatory document that controls the creation
    // of the index -
    // - you can write e.g.
    // ! book.EnsureIndex(_JsonFast('{ orderDate: 1 }'),null)
    // ! book.EnsureIndex(_ObjFast(['orderDate',1]),null)
    procedure EnsureIndex(const Keys, Options: variant); overload;
    /// creates an index on the specified field(s) if the index does
    // not already exist
    // - Keys are the correspondiong field names
    // - you can write e.g. to create an ascending index on a given field:
    // ! book.EnsureIndex(['orderDate']);
    procedure EnsureIndex(const Keys: array of RawUTF8; Ascending: boolean=true;
      Unique: boolean=false); overload;
    /// drops the entire collection from the database
    // - once dropped, this TMongoCollection instance will be freed: never
    // use this instance again after success (i.e. returned '')
    // - in case of error, a textual message will be returned as result
    // - once dropped, this collection will be removed from the parent
    // Database.Collection[] internal list
    // - Warning: this method obtains a write lock on the affected database
    // and will block other operations until it has completed
    function Drop: RawUTF8;

    /// calculate the number of documents in the collection
    // - be aware that this method may be somewhat slow for huge collections,
    // since a full scan of an index is to be performed: if your purpose is
    // to ensure that a collection contains items, use rather IsEmpty method
    function Count: Int64;
    /// calculate the number of documents in the collection that match
    // a specific query
    // - Criteria can specify the query selector as a BSONVariant/TDocVariant
    function FindCount(const Query: variant): Int64; overload;
    /// calculate the number of documents in the collection that match
    // a specific query
    // - Criteria can specify the query selector as (extended) JSON and
    // parameters:
    // ! FindCount('{name:?,age:{$gt:?}}',[],['John',21]);
    // ! FindCount('{ ord_dt: { $gt: new Date(?) } }',[],[trunc(Now)-7]);
    // - optional MaxNumberToReturn can specify a limit for the search (e.g. if
    // you do not want an exact count, but only check for a specific limit)
    // - optional NumberToSkip can specify the number of matching documents
    // to skip before counting
    function FindCount(Criteria: PUTF8Char; const Args,Params: array of const;
      MaxNumberToReturn: integer=0; NumberToSkip: integer=0): Int64; overload;
    /// returns TRUE if the collection has no document, FALSE otherwise
    // - is much faster than Count, especially for huge collections
    function IsEmpty: boolean;
    /// calculate aggregate values using the MongoDB aggregation framework
    // and return the result as a TDocVariant instance
    // - the Aggregation Framework was designed to be more efficient than the
    // alternative map-reduce pattern, and is available since MongoDB 2.2 -
    // see http://docs.mongodb.org/manual/reference/command/aggregate
    // - you should specify the aggregation pipeline as a list of JSON object
    // operators (without the [..]) - for reference of all available phases,
    // see http://docs.mongodb.org/manual/core/aggregation-pipeline
    // - if the server sent back no {result:...} member, will return null
    // - if the server sent back one item as {result:[{..}]}, will return
    // this single item as a TDocVariant
    // - if the server sent back several items as {result:[{..},{..}]}, will
    // return a dvArray kind of TDocVariant
    function AggregateDoc(Operators: PUTF8Char; const Params: array of const): variant; overload;
    /// calculate JSON aggregate values using the MongoDB aggregation framework
    // - the Aggregation Framework was designed to be more efficient than the
    // alternative map-reduce pattern, and is available since MongoDB 2.2 -
    // see http://docs.mongodb.org/manual/reference/command/aggregate
    // - you should specify the aggregation pipeline as a list of JSON object
    // operators (without the [..]) - for reference of all available phases,
    // see http://docs.mongodb.org/manual/core/aggregation-pipeline
    // - for instance, the following will return as JSON a collection sorted in
    // descending order according by the age field and then in ascending order
    // according to the value in the posts field
    // ! AggregateJSON('{ $sort : { age : -1, posts: 1 } }',[])
    function AggregateJSON(Operators: PUTF8Char; const Params: array of const;
      Mode: TMongoJSONMode=modMongoStrict): RawUTF8; overload;
    /// calculate aggregate values using the MongoDB aggregation framework
    // and return the result as a TDocVariant instance
    // - overloaded method to specify the pipeline as a BSON raw document
    // as detailed by http://docs.mongodb.org/manual/core/aggregation-pipeline
    function AggregateDocFromVariant(const pipelineArray: variant): variant;
    /// calculate JSON aggregate values using the MongoDB aggregation framework
    // - overloaded method to specify the pipeline as a BSON raw document
    // as detailed by http://docs.mongodb.org/manual/core/aggregation-pipeline
    function AggregateJSONFromVariant(const pipelineArray: variant;
      Mode: TMongoJSONMode=modMongoStrict): RawUTF8; overload;
    /// calculate aggregate values using the MongoDB aggregation framework
    // and return the result as a TDocVariant instance
    // - overloaded method to specify the pipeline as a JSON text object
    // as detailed by http://docs.mongodb.org/manual/core/aggregation-pipeline
    // - for instance, the following will return the maximum _id value of
    // the collection:
    // ! AggregateDoc('{$group:{_id:null,max:{$max:"$_id"}}}').max
    function AggregateDocFromJson(const PipelineJSON: RawUTF8): variant;
    /// calculate JSON aggregate values using the MongoDB aggregation framework
    // - overloaded method to specify the pipeline as a JSON text object
    // as detailed by http://docs.mongodb.org/manual/core/aggregation-pipeline
    function AggregateJSONFromJson(const PipelineJSON: RawUTF8;
      Mode: TMongoJSONMode=modMongoStrict): RawUTF8; overload;
  published
    /// the collection name
    property Name: RawUTF8 read fName;
    /// the full collection name, e.g. 'dbname.collectionname'
    property FullCollectionName: RawUTF8 read fFullCollectionName;
    /// the associated MongoDB database instance
    property Database: TMongoDatabase read fDatabase;
  end;

  /// exception type used for MongoDB process, once connected
  EMongoConnectionException = class(EMongoException)
  protected
    fConnection: TMongoConnection;
  public
    /// initialize the Exception for a given request
    constructor Create(const aMsg: string; aConnection: TMongoConnection); reintroduce; overload;
    /// initialize the Exception for a given request
    constructor CreateUTF8(const Format: RawUTF8; const Args: array of const;
      aConnection: TMongoConnection); reintroduce;
  published
    /// the associated connection
    property Connection: TMongoConnection read fConnection;
  end;

  EMongoDatabaseException = class(EMongoConnectionException)
  protected
    fDatabase: TMongoDatabase;
  public
    /// initialize the Exception for a given request
    constructor Create(const aMsg: string; aDatabase: TMongoDatabase); reintroduce; overload;
    /// initialize the Exception for a given request
    constructor CreateUTF8(const Format: RawUTF8; const Args: array of const;
      aDatabase: TMongoDatabase); reintroduce;
    {$ifndef NOEXCEPTIONINTERCEPT}
    /// used to customize the exception log to contain information about the Query
    // - it will log the database parameters
    function CustomLog(WR: TTextWriter; const Context: TSynLogExceptionContext): boolean; override;
    {$endif}
  published
    /// the associated Database
    property Database: TMongoDatabase read fDatabase;
  end;

  /// exception type used for MongoDB query process
  EMongoRequestException = class(EMongoConnectionException)
  protected
    fRequest: TMongoRequest;
    fError: TMongoReplyCursor;
    fErrorDoc: variant;
    function GetErrorDoc: variant;
  public
    /// initialize the Exception for a given request
    constructor Create(const aMsg: string; aConnection: TMongoConnection;
      aRequest: TMongoRequest=nil); reintroduce; overload;
    /// initialize the Exception for a given request
    constructor CreateUTF8(const Format: RawUTF8; const Args: array of const;
      aConnection: TMongoConnection; aRequest: TMongoRequest); reintroduce;
    /// initialize the Exception for a given request
    constructor Create(const aMsg: string; aConnection: TMongoConnection;
      aRequest: TMongoRequest; const aError: TMongoReplyCursor); reintroduce; overload;
    /// initialize the Exception for a given request
    constructor Create(const aMsg: string; aConnection: TMongoConnection;
      aRequest: TMongoRequest; const aErrorDoc: TDocVariantData); reintroduce; overload;
    {$ifndef NOEXCEPTIONINTERCEPT}
    /// used to customize the exception log to contain information about the Query
    // - it will log both the failing request and the returned error message
    function CustomLog(WR: TTextWriter; const Context: TSynLogExceptionContext): boolean; override;
    {$endif}
    /// the associated error reply document
    property ErrorReply: TMongoReplyCursor read fError;
  published
    /// the associated error reply document, as a TDocVariant instance
    // - will return the first document available in ErrorReply, or the supplied
    // aErrorDoc: TDocVariantData instance
    property ErrorDoc: Variant read GetErrorDoc;
  end;

  /// exception type used for MongoDB query process after an Operating System
  // error (e.g. in case of socket error)
  EMongoRequestOSException = class(EMongoRequestException)
  protected
    fSystemLastError: cardinal;
  public
    /// initialize the Exception for a given request, including the last
    // error message retrieved from the operating system
    // - if such an exception is raised, you can use SystemLastError property
    // to retrieve the corresponding Operating System error code
    constructor Create(const aMsg: string; aConnection: TMongoConnection;
      aRequest: TMongoRequest=nil); reintroduce;
    /// contain the associated Operating System last error code
    // - will specify e.g. the kind of communication/socket error
    property SystemLastError: cardinal read fSystemLastError;
  end;

{$M-}


/// ready-to-be displayed text of a TMongoOperation item
function ToText(op: TMongoOperation): PShortString; overload;

/// ready-to-be displayed text of a TMongoClientWriteConcern item
function ToText(wc: TMongoClientWriteConcern): PShortString; overload;

/// ready-to-be displayed text of a TMongoClientReplicaSetReadPreference item
function ToText(pref: TMongoClientReplicaSetReadPreference): PShortString; overload;


implementation

// used by TBSONElement.ToVariant() method and BSONToDoc() procedure
procedure BSONItemsToDocVariant(Kind: TBSONElementType; BSON: PByte;
  var Doc: TDocVariantData; Option: TBSONDocArrayConversion);
const OPTIONS: array[TBSONDocArrayConversion] of TDocVariantOptions =
    ([],[dvoReturnNullForUnknownProperty],
        [dvoReturnNullForUnknownProperty,dvoValueCopiedByReference],
        [dvoReturnNullForUnknownProperty,dvoInternNames],
        [dvoReturnNullForUnknownProperty,dvoValueCopiedByReference,dvoInternNames]);
var k: TDocVariantKind;
    i,n,cap: integer;
    intnames: TRawUTF8Interning;
    items: array[0..63] of TBSONElement;
begin // very fast optimized code
  if BSON=nil then
    TVarData(Doc).VType := varNull else begin
    intnames := nil;
    case Kind of
    betDoc: begin
      k := dvObject;
      if dvoInternNames in Doc.Options then
        intnames := DocVariantType.InternNames;
    end;
    betArray:
      k := dvArray;
    else exit; // leave Doc=varEmpty
    end;
    Doc.Init(OPTIONS[Option],k);
    cap := 0;
    repeat // will handle up to 64 TBSONElement per loop (via items[])
      n := 0;
      while items[n].FromNext(BSON) do begin
        inc(n);
        if n=length(items) then
          break; // avoid buffer overflow
      end;
      if n=0 then
        break;
      inc(cap,n); // pre-allocate Doc.Names[]/Values[]
      if Doc.Capacity<cap then
        Doc.Capacity := NextGrow(cap); // faster for huge arrays
      for i := 0 to n-1 do begin
        if Kind=betDoc then
          if intnames<>nil then
            intnames.Unique(Doc.Names[i+Doc.Count],items[i].Name,items[i].NameLen) else
            FastSetString(Doc.Names[i+Doc.Count],items[i].Name,items[i].NameLen);
        items[i].ToVariant(Doc.Values[i+Doc.Count],Option);
      end;
      Doc.SetCount(Doc.Count+n);
    until (BSON=nil) or (BSON^=byte(betEOF));
  end;
end;


{ TBSONElement }

var
  /// size (in bytes) of a BSON element
  // - equals -1 for varying elements
  BSON_ELEMENTSIZE: array[TBSONElementType] of integer = (
    //betEOF, betFloat, betString, betDoc, betArray, betBinary,
      0,     sizeof(Double), -1,     -1,     -1,       -1,
    //betDeprecatedUndefined, betObjectID, betBoolean, betDateTime,
      0,                    sizeof(TBSONObjectID), 1, sizeof(Int64),
    //betNull, betRegEx, betDeprecatedDbptr, betJS, betDeprecatedSymbol,
      0,        -1,           -1,             -1,        -1,
    //betJSScope, betInt32, betTimestamp, betInt64, betDecimal128
      -1, sizeof(integer), sizeof(Int64), SizeOf(Int64), Sizeof(TDecimal128));

  /// types which do not have an exact equivalency to a standard variant
  // type will be mapped as varUnknown - and will be changed into
  // BSONVariantType.VarType
  BSON_ELEMENTTYPES: array[TBSONElementType] of word = (
    //betEOF, betFloat, betString, betDoc, betArray, betBinary,
    varEmpty, varDouble, varString, varUnknown, varUnknown, varUnknown,
    //betDeprecatedUndefined, betObjectID, betBoolean, betDateTime,
    varEmpty, varUnknown, varBoolean, varDate,
    //betNull, betRegEx, betDeprecatedDbptr, betJS, betDeprecatedSymbol,
    varNull, varUnknown, varUnknown, varUnknown, varUnknown,
    //betJSScope, betInt32, betTimestamp, betInt64, betDecimal128
    varUnknown, varInteger, varUnknown, varInt64, varUnknown);

function TBSONElement.ToVariant(DocArrayConversion: TBSONDocArrayConversion): variant;
begin
  ToVariant(Result,DocArrayConversion);
end;

procedure TBSONElement.ToVariant(var result: variant;
  DocArrayConversion: TBSONDocArrayConversion);
var res: TVarData absolute result;
    resBSON: TBSONVariantData absolute result;
begin
  VarClear(result);
  res.VAny := nil; // avoid GPF below
  case Kind of
  betFloat:
    res.VDouble := unaligned(PDouble(Element)^);
  betString:
    FastSetString(RawUTF8(res.VAny),Data.Text,Data.TextLen);
  betJS, betDeprecatedSymbol:
    FastSetString(RawUTF8(resBSON.VText),Data.Text,Data.TextLen);
  betDoc, betArray:
    if DocArrayConversion=asBSONVariant then
      SetString(TBSONDocument(resBSON.VBlob),PAnsiChar(Element),ElementBytes) else begin
      BSONItemsToDocVariant(Kind,Data.DocList,TDocVariantData(result),DocArrayConversion);
      exit;
    end;
  betBinary, betRegEx, betDeprecatedDbptr, betJSScope, betTimestamp, betDecimal128:
    SetString(RawByteString(resBSON.VBlob),PAnsiChar(Element),ElementBytes);
  betObjectID:
    resBSON.VObjectID := PBSONObjectID(Element)^;
  betBoolean:
    res.VBoolean := PBoolean(Element)^;
  betDateTime:
    res.VDate := UnixMSTimeToDateTime(PUnixMSTime(Element)^);
  betInt32:
    res.VInteger := PInteger(Element)^;
  betInt64:
    res.VInt64 := PInt64(Element)^;
  // betNull, betDeprecatedUndefined, betMinKey or betMaxKey has no data
  end;
  res.VType := BSON_ELEMENTTYPES[Kind];
  if res.VType=varUnknown then begin
    resBSON.VType := BSONVariantType.VarType;
    resBSON.VKind := Kind;
  end;
end;

function TBSONElement.ToInteger(const default: Int64=0): Int64;
begin
  case Kind of
  betBoolean:
    result := PByte(Element)^;
  betFloat:
    result := Trunc(unaligned(PDouble(Element)^));
  betInt32:
    result := PInteger(Element)^;
  betInt64:
    result := PInt64(Element)^;
  else
    result := default;
  end;
end;

function TBSONElement.ToRawUTF8: RawUTF8;
procedure ComplexType;
var V: variant;
    wasString: boolean;
begin
  ToVariant(V);
  VariantToUTF8(V,result,wasString);
end;
begin
  case Kind of
  betFloat:
    DoubleToStr(unaligned(PDouble(Element)^),result);
  betString:
    FastSetString(result,Data.Text,Data.TextLen);
  betInt32:
    Int32ToUtf8(PInteger(Element)^,result);
  betInt64:
    Int64ToUtf8(PInt64(Element)^,result);
  betDecimal128:
    PDecimal128(Element)^.ToText(result);
  else ComplexType;
  end;
end;

function TBSONElement.DocItemToVariant(const aName: RawUTF8; var aValue: variant;
  DocArrayConversion: TBSONDocArrayConversion): boolean;
var item: TBSONElement;
begin
  if (Kind in [betDoc,betArray]) and item.FromSearch(Data.DocList,aName) then begin
    item.ToVariant(aValue,DocArrayConversion);
    result := true;
  end else
    result := false;
end;

function TBSONElement.DocItemToRawUTF8(const aName: RawUTF8): RawUTF8;
var item: TBSONElement;
begin
  if (Kind in [betDoc,betArray]) and item.FromSearch(Data.DocList,aName) then
    result := item.ToRawUTF8 else
    result := '';
end;

function TBSONElement.DocItemToInteger(const aName: RawUTF8; const default: Int64): Int64;
var item: TBSONElement;
begin
  if (Kind in [betDoc,betArray]) and item.FromSearch(Data.DocList,aName) then
    result := item.ToInteger(default) else
    result := default;
end;

procedure TBSONElement.AddMongoJSON(W: TTextWriter; Mode: TMongoJSONMode);
label bin,regex;
begin
  case integer(Kind) of
  ord(betFloat):
    W.AddDouble(unaligned(PDouble(Element)^));
  ord(betString), ord(betJS), ord(betDeprecatedSymbol): begin
    W.Add('"');
    W.AddJSONEscape(Data.Text,Data.TextLen);
    W.Add('"');
  end;
  ord(betDoc), ord(betArray):
    BSONListToJSON(Data.DocList,Kind,W,Mode);
  ord(betObjectID): begin
    W.AddShort(BSON_JSON_OBJECTID[false,Mode]);
    W.AddBinToHex(Element,SizeOf(TBSONObjectID));
    W.AddShort(BSON_JSON_OBJECTID[true,Mode]);
  end;
  ord(betDeprecatedUndefined):
    W.AddShort(BSON_JSON_UNDEFINED[Mode=modMongoShell]);
  ord(betBinary):
    case Mode of
    modNoMongo:
      W.WrBase64(Data.Blob,Data.BlobLen,true);
    modMongoStrict: begin
      W.AddShort(BSON_JSON_BINARY[false,false]);
      W.WrBase64(Data.Blob,Data.BlobLen,false);
      W.AddShort(BSON_JSON_BINARY[false,true]);
      W.AddBinToHex(@Data.BlobSubType,1);
      W.AddShort('"}');
    end;
    modMongoShell: begin
      W.AddShort(BSON_JSON_BINARY[true,false]);
      W.AddBinToHex(@Data.BlobSubType,1);
      W.AddShort(BSON_JSON_BINARY[true,true]);
      W.WrBase64(Data.Blob,Data.BlobLen,false);
      W.AddShort('")');
    end;
    end;
  ord(betRegEx):
    case Mode of
    modNoMongo:
bin:W.WrBase64(Element,ElementBytes,true);
    modMongoStrict:
      goto regex;
    modMongoShell:
      if (PosChar(Data.RegEx,'/')=nil) and
         (PosChar(Data.RegExOptions,'/')=nil) then begin
        W.Add('/');
        W.AddNoJSONEscape(Data.RegEx,Data.RegExLen);
        W.Add('/');
        W.AddNoJSONEscape(Data.RegExOptions,Data.RegExOptionsLen);
      end else begin
regex:  W.AddShort(BSON_JSON_REGEX[0]);
        W.AddJSONEscape(Data.RegEx,Data.RegExLen);
        W.AddShort(BSON_JSON_REGEX[1]);
        W.AddJSONEscape(Data.RegExOptions,Data.RegExOptionsLen);
        W.AddShort(BSON_JSON_REGEX[2]);
      end;
    end;
  ord(betDeprecatedDbptr):
    goto bin; // no specific JSON construct for this deprecated item
  ord(betJSScope):
    goto bin; // no specific JSON construct for this item yet
  ord(betTimestamp):
    goto bin; // internal content will always be written as raw binary
  ord(betBoolean):
    W.Add(PBoolean(Element)^);
  ord(betDateTime): begin
    W.AddShort(BSON_JSON_DATE[Mode,false]);
    W.AddUnixMSTime(Element,false);
    W.AddShort(BSON_JSON_DATE[Mode,true]);
  end;
  ord(betNull):
    W.AddShort('null');
  ord(betInt32):
    W.Add(PInteger(Element)^);
  ord(betInt64):
    W.Add(PInt64(Element)^);
  ord(betDecimal128): begin
    W.AddShort(BSON_JSON_DECIMAL[false,Mode]);
    PDecimal128(Element)^.AddText(W);
    W.AddShort(BSON_JSON_DECIMAL[true,Mode]);
  end;
  betMinKey:
    W.AddShort(BSON_JSON_MINKEY[Mode=modMongoShell]);
  betMaxKey:
    W.AddShort(BSON_JSON_MAXKEY[Mode=modMongoShell]);
  else
    raise EBSONException.CreateUTF8('TBSONElement.AddMongoJSON: unexpected type %',
      [integer(Kind)]);
  end;
end;

procedure TBSONElement.FromVariant(const aName: RawUTF8; const aValue: Variant;
  var aTemp: RawByteString);
const ELEMKIND: array[varEmpty..varWord64] of TBSONElementType = (
  betEOF, betNull, betInt32, betInt32, betFloat, betFloat, betFloat, betDateTime,
  betString, betEOF, betEOF, betBoolean, betEof, betEOF, betEOF, betEOF,
  betInt32, betInt32, betInt32, betInt64, betInt64, betInt64);
var v: PVarData;
    vbson: PBSONVariantData absolute v;
    vdoc: PDocVariantData absolute v;
    vt: cardinal;
label str, st2;
begin
  v := @aValue;
  while v.VType=varByRef or varVariant do
    v := v.VPointer;
  FillCharFast(self,sizeof(self),0);
  Name := pointer(aName);
  NameLen := length(aName);
  vt := v.VType;
  case vt of
  0..varDate,varBoolean..high(ELEMKIND): begin // simple types
    Element := @Data.InternalStorage;
    Kind := ELEMKIND[vt];
    case Kind of
    betFloat:
      unaligned(PDouble(Element)^) := double(aValue);
    betDateTime:
      PUnixMSTime(Element)^ := DateTimeToUnixMSTime(v.VDate);
    betBoolean:
      PBoolean(Element)^ := v.VBoolean;
    betInt32:
      if not VariantToInteger(aValue,PInteger(Element)^) then
        raise EBSONException.Create('TBSONElement.FromVariant(betInt32)');
    betInt64:
      if not VariantToInt64(aValue,PInt64(Element)^) then
        raise EBSONException.Create('TBSONElement.FromVariant(betInt64)');
    end;
    ElementBytes := BSON_ELEMENTSIZE[Kind];
  end;
  varString:
    if (v.VAny<>nil) and
       (PInteger(v.VAny)^ and $ffffff=JSON_SQLDATE_MAGIC) and
       Iso8601CheckAndDecode(PUTF8Char(v.VAny)+3,Length(RawUTF8(v.VAny))-3,
         PDateTime(@Data.InternalStorage)^) then begin
      // recognized TTextWriter.AddDateTime(woDateTimeWithMagic) ISO-8601 format
      Element := @Data.InternalStorage;
      Kind := betDateTime;
      ElementBytes := BSON_ELEMENTSIZE[betDateTime];
    end else begin
      Kind := betString;
      Data.Text := v.VAny;
      Data.TextLen := Length(RawUTF8(v.VAny));
st2:  ElementBytes := Data.TextLen+1;
      if v.VAny=nil then
        Data.InternalStorage := 1 else
        Element := nil; // special case handled by TBSONWriter.BSONWrite()
    end;
  {$ifdef HASVARUSTRING}
  varUString: begin
    RawUnicodeToUtf8(v.VAny,length(UnicodeString(v.VAny)),RawUTF8(aTemp));
    goto str;
  end;
  {$endif}
  varOleStr: begin
    RawUnicodeToUtf8(v.VAny,length(WideString(v.VAny)),RawUTF8(aTemp));
str:Kind := betString;
    Data.Text := pointer(aTemp);
    Data.TextLen := Length(aTemp);
    goto st2;
  end;
  else
  if vt=cardinal(BSONVariantType.VarType) then begin
    Kind := vbson.VKind;
    case Kind of
    betObjectID: FromBSON(@vbson.VObjectID); // stored inlined
    else         FromBSON(vbson.VBlob); // complex type stored as a RawByteString
    end;
    if ElementBytes<0 then
      raise EBSONException.CreateUTF8('TBSONElement.FromVariant(bson,%)',[ToText(Kind)^]);
  end else
  if vt=cardinal(DocVariantVType) then begin
    with TBSONWriter.Create(TRawByteStringStream) do // inlined BSON()
    try
      BSONWriteDoc(vdoc^);
      ToBSONDocument(aTemp);
    finally
      Free;
    end;
    if dvoIsObject in vdoc.Options then
      Kind := betDoc else
    if dvoIsArray in vdoc.Options then
      Kind := betArray else
      raise EBSONException.CreateUTF8('TBSONElement.FromVariant(doc,%)',[ToText(vdoc.Kind)^]);
    FromBSON(pointer(aTemp));
    if ElementBytes<0 then
      raise EBSONException.CreateUTF8('TBSONElement.FromVariant(docbson,%)',[ToText(Kind)^]);
  end else
    raise EBSONException.CreateUTF8('TBSONElement.FromVariant(VType=%)',[v.VType]);
  end;
end;

function TBSONElement.FromDocument(const doc: TBSONDocument): boolean;
var n: integer;
begin
  FillCharFast(self,sizeof(self),0);
  n := length(doc);
  if (n>=4) and (PInteger(doc)^=n) then begin
    Kind := betDoc;
    FromBSON(pointer(doc));
    result := true;
  end else
    result := false;
end;

const
  NULCHAR: AnsiChar = #0;

procedure TBSONElement.FromBSON(bson: PByte);
begin // see http://bsonspec.org/#/specification
  Element := bson;
  case Kind of // handle variable-size storage
  betString, betJS, betDeprecatedSymbol: begin  // "\x02" e_name string
    ElementBytes := PInteger(bson)^+sizeof(integer); // int32 (byte*) "\x00"
    Data.TextLen := PInteger(bson)^-1;
    inc(bson,sizeof(integer));
    Data.Text := pointer(bson);
  end;
  betDoc, betArray: begin  // "\x03" e_name document
    ElementBytes := PInteger(bson)^;
    inc(bson,sizeof(integer)); // points to a "e_list #0"
    Data.DocList := bson;
  end;
  betBinary: begin         // "\x05" e_name int32 subtype (byte*)
    ElementBytes := PInteger(bson)^+(sizeof(integer)+1);
    Data.BlobLen := PInteger(bson)^;
    inc(bson,sizeof(integer));
    Data.BlobSubType := TBSONElementBinaryType(bson^);
    inc(bson);
    Data.Blob := bson;
  end;
  betRegEx: begin          // "\x0B" e_name cstring cstring
    Data.RegEx := Element;
    Data.RegExLen := StrLen(Data.RegEx);
    Data.RegExOptions := Data.RegEx+Data.RegExLen+1;
    Data.RegExOptionsLen := StrLen(Data.RegExOptions);
    ElementBytes := Data.RegExLen+Data.RegExOptionsLen+2;
  end;
  betJSScope: begin       // "\x0F" e_name  int32 string document
    ElementBytes := PInteger(bson)^;
    inc(bson,sizeof(integer));
    Data.JavaScriptLen := PInteger(bson)^-1;
    inc(bson,sizeof(integer));
    Data.JavaScript := pointer(bson);
    inc(bson,Data.JavaScriptLen+1);
    Data.ScopeDocument := bson;
  end;
  else
    if Kind>high(BSON_ELEMENTSIZE) then // e.g. betMinKey betMaxKey
      ElementBytes := 0 else
      ElementBytes := BSON_ELEMENTSIZE[Kind]; // fixed size storage
  end;
end;

function TBSONElement.FromNext(var BSON: PByte): boolean;
begin
  if BSON=nil then begin
    result := false;
    exit;
  end;
  Kind := TBSONElementType(BSON^);
  case integer(Kind) of
  ord(betEOF):
    result := false;
  ord(betFloat)..ord(betDecimal128),betMinKey,betMaxKey: begin
    inc(BSON);
    Name := PUTF8Char(BSON);
    NameLen := StrLen(PUTF8Char(BSON));
    inc(BSON,NameLen+1);
    FromBSON(BSON);
    if ElementBytes<0 then
      raise EBSONException.CreateUTF8(
        'TBSONElement.FromNext: unexpected size % for type %',[ElementBytes,ord(Kind)]);
    inc(BSON,ElementBytes);
    inc(Index);
    result := true;
  end;
  else raise EBSONException.CreateUTF8('TBSONElement.FromNext: unexpected type %',
    [ord(Kind)]);
  end;
end;

function TBSONElement.FromSearch(BSON: PByte; const aName: RawUTF8): boolean;
begin
  result := true;
  while FromNext(BSON) do
    if IdemPropNameU(aName,Name,NameLen) then
      exit;
  result := false;
end;


{ TBSONIterator }

function TBSONIterator.Init(const doc: TBSONDocument; kind: TBSONElementType): boolean;
var n: integer;
begin
  FillCharFast(self,sizeof(self),0);
  n := length(doc);
  if (kind in [betDoc,betArray]) and (n>=4) and (PInteger(doc)^=n) then begin
    Item.Kind := kind;
    Item.FromBSON(pointer(doc));
    fBson := Item.Data.DocList;
    result := true;
  end else
    result := false;
end;

function TBSONIterator.Next: boolean;
begin
  result := Item.FromNext(fBson);
end;


function BSONParseLength(var BSON: PByte; ExpectedBSONLen: integer=0): integer;
begin
  if (BSON=nil) or
     ((ExpectedBSONLen<>0) and (PInteger(BSON)^<>ExpectedBSONLen)) then
     raise EBSONException.Create('Incorrect supplied BSON document content');
  result := PInteger(BSON)^;
  inc(PInteger(BSON));
end;

function BSONParseNextElement(var BSON: PByte; var name: RawUTF8; var element: variant;
  DocArrayConversion: TBSONDocArrayConversion=asBSONVariant): boolean;
var item: TBSONElement;
begin
  result := item.FromNext(BSON);
  if result then begin
    FastSetString(name,item.Name,item.NameLen);
    item.ToVariant(element,DocArrayConversion);
  end;
end;

function BSONPerIndexElement(BSON: PByte; index: integer; var item: TBSONElement): boolean;
begin
  result := true;
  if (index>=0) and (BSON<>nil) and (BSONParseLength(BSON)<>0) then
    while item.FromNext(BSON) do
      if index=0 then
        exit else
        dec(index);
  result := false;
end;

procedure BSONToDoc(BSON: PByte; var Result: Variant; ExpectedBSONLen: integer;
  Option: TBSONDocArrayConversion);
begin
  if Option=asBSONVariant then
    raise EBSONException.Create('BSONToDoc(option=asBSONVariant) is not allowed');
  VarClear(result);
  BSONParseLength(BSON,ExpectedBSONLen);
  BSONItemsToDocVariant(betDoc,BSON,TDocVariantData(Result),Option);
end;

function BSONDocumentToDoc(const BSON: TBSONDocument; Option: TBSONDocArrayConversion): variant;
begin
  BSONToDoc(pointer(BSON),result,length(BSON));
end;

procedure BSONListToJSON(BSONList: PByte; Kind: TBSONElementType;
  W: TTextWriter; Mode: TMongoJSONMode);
var item: TBSONElement;
begin
  case Kind of
  betDoc:
    if BSONList^=byte(betEOF) then
      W.Add('{','}') else begin
      W.Add('{');
      while item.FromNext(BSONList) do begin
        if Mode=modMongoShell then begin
          W.AddNoJSONEscape(item.Name,item.NameLen);
          W.Add(':');
        end else
          W.AddProp(item.Name,item.NameLen);
        item.AddMongoJSON(W,Mode);
        W.Add(',');
      end;
      W.CancelLastComma;
      W.Add('}');
    end;
  betArray: begin
    W.Add('[');
    while item.FromNext(BSONList) do begin
      item.AddMongoJSON(W,Mode);
      W.Add(',');
    end;
    W.CancelLastComma;
    W.Add(']');
  end;
  else raise EBSONException.CreateUTF8('BSONListToJSON(Kind=%)',[ord(Kind)]);
  end;
end;

function BSONDocumentToJSON(const BSON: TBSONDocument;
  Mode: TMongoJSONMode=modMongoStrict): RawUTF8;
begin
  result := BSONToJSON(pointer(BSON),betDoc,length(BSON),Mode);
end;

function BSONToJSON(BSON: PByte; Kind: TBSONElementType; ExpectedBSONLen: integer;
  Mode: TMongoJSONMode): RawUTF8;
var W: TTextWriter;
    tmp: TTextWriterStackBuffer;
begin
  BSONParseLength(BSON,ExpectedBSONLen);
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    BSONListToJSON(BSON,Kind,W,Mode);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure AddMongoJSON(const Value: variant; W: TTextWriter; Mode: TMongoJSONMode);
  procedure AddCustom;
  var item: TBSONElement;
      temp: RawByteString;
  begin
    item.FromVariant('',Value,temp);
    item.AddMongoJSON(W,Mode);
  end;
begin
  if TVarData(Value).VType<$10F then
    W.AddVariant(Value,twJSONEscape) else
    AddCustom;
end;

function VariantSaveMongoJSON(const Value: variant; Mode: TMongoJSONMode): RawUTF8;
var W: TTextWriter;
    tmp: TTextWriterStackBuffer;
begin
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    AddMongoJSON(Value,W,Mode);
    W.SetText(result);
  finally
    W.Free;
  end;
end;


{ TBSONWriter }

procedure TBSONWriter.CancelAll;
begin
  inherited;
  fDocumentCount := 0;
end;

procedure TBSONWriter.WriteCollectionName(Flags: integer; const CollectionName: RawUTF8);
begin
  Write4(Flags);
  if CollectionName='' then
    raise EBSONException.Create('Missing collection name');
  Write(pointer(CollectionName),length(CollectionName)+1); // +1 for #0
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; elemtype: TBSONElementType);
begin
  Write1(ord(elemtype));
  if name='' then
    write1(0) else // write only #0
    {$ifdef HASINLINE}
    Write(pointer(name),length(name)+1); // +1 for #0
    {$else}
    Write(pointer(name),PInteger(PtrInt(name)-sizeof(integer))^+1); // +1 for #0
    {$endif}
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: integer);
begin
  BSONWrite(name,betInt32);
  Write4(value);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: Double);
begin
  BSONWrite(name,betFloat);
  Write8(value);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: boolean);
begin
  BSONWrite(name,betBoolean);
  Write1(ord(value));
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: Int64);
begin
  if (value>=low(integer)) and (value<=high(integer)) then begin
    BSONWrite(name,betInt32);
    Write4(value);
  end else begin
    BSONWrite(name,betInt64);
    Write8(value);
  end;
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: TBSONObjectID);
begin
  BSONWrite(name,betObjectID);
  Write(@value,sizeof(value));
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: TDecimal128);
begin
  BSONWrite(name,betDecimal128);
  Write(@value,sizeof(value));
end;

procedure TBSONWriter.BSONWriteRegEx(const name: RawUTF8;
  const RegEx,Options: RawByteString);
begin
  BSONWrite(name,betRegEx); // cstring cstring
  Write(pointer(RegEx),length(RegEx));
  Write1(0);
  Write(pointer(Options),length(Options));
  Write1(0);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: RawUTF8;
  isJavaScript: boolean=false);
const TYP: array[boolean] of TBSONElementType = (betString,betJS);
var L: integer;
begin
  BSONWrite(name,TYP[isJavaScript]);
  L := length(value)+1; // +1 for ending #0
  Write4(L);
  if L=1 then
    Write1(0) else
    Write(pointer(value),L);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; value: PUTF8Char);
var L: integer;
begin
  BSONWrite(name,betString);
  L := StrLen(Value)+1;
  Write4(L);
  if L=1 then
    Write1(0) else
    Write(value,L);
end;

procedure TBSONWriter.BSONWriteString(const name: RawUTF8; value: PUTF8Char; valueLen: integer);
begin
  BSONWrite(name,betString);
  inc(valueLen);
  Write4(valueLen);
  if valueLen=1 then
    Write1(0) else
    Write(value,valueLen);
end;

procedure TBSONWriter.BSONWriteDateTime(const name: RawUTF8; const value: TDateTime);
var ms: TUnixMSTime;
begin
  ms := DateTimeToUnixMSTime(value);
  BSONWrite(name,betDateTime);
  Write8(ms);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; Data: pointer; DataLen: integer);
begin
  BSONWrite(name,betBinary);
  Write4(DataLen);
  Write1(ord(bbtGeneric));
  Write(Data,DataLen);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const elem: TBSONElement);
begin
  BSONWrite(name,elem.Kind);
  if (elem.Element=nil) and // handle special case of TBSONElement.FromVariant()
     (elem.Kind in [betString,betJS,betDeprecatedSymbol]) then begin
    Write4(elem.Data.TextLen+1); // int32 (byte*) "\x00"
    Write(elem.Data.Text,elem.Data.TextLen+1);
  end else
    Write(elem.Element,elem.ElementBytes);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const bson: TBSONVariantData);
begin
  case bson.VKind of
  betObjectID:
    BSONWrite(name,bson.VObjectID);
  else begin
    BSONWrite(name,bson.VKind);
    WriteBinary(RawByteString(bson.VBlob));
  end;
  end;
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const doc: TDocVariantData);
begin
  if dvoIsObject in doc.Options then
    BSONWrite(name,betDoc) else
  if dvoIsArray in doc.Options then
    BSONWrite(name,betArray) else
    raise EBSONException.Create('Undefined nested document');
  BSONWriteDoc(doc);
end;

procedure TBSONWriter.BSONWriteArray(const kind: TBSONElementType);
begin
  BSONWrite(UInt32ToUtf8(fDocumentArray),kind);
  inc(fDocumentArray);
  if kind in [betDoc,betArray] then
    BSONDocumentBegin;
end;

procedure TBSONWriter.BSONDocumentBegin;
begin
  if fDocumentStack>=Length(fDocumentStackOffset) then
    SetLength(fDocumentStackOffset,NextGrow(fDocumentStack));
  fDocumentStackOffset[fDocumentStack] := TotalWritten;
  inc(fDocumentStack);
  Write4(0);
end;

procedure TBSONWriter.BSONDocumentBegin(const name: RawUTF8; kind: TBSONElementType);
begin
  if not (kind in [betDoc,betArray]) then
    raise EBSONException.Create('BSONDocumentBegin(?)');
  BSONWrite(name,kind);
  BSONDocumentBegin;
end;

procedure TBSONWriter.BSONDocumentBeginInArray(const name: RawUTF8; kind: TBSONElementType);
begin
  if fDocumentArray>0 then
    BSONDocumentEnd;
  BSONWriteArray(kind);
  BSONDocumentBegin(name);
end;

procedure TBSONWriter.BSONDocumentEnd(CloseNumber: integer; WriteEndingZero: boolean);
begin
  while CloseNumber>0 do begin
    if (CloseNumber>1) or WriteEndingZero then
      Write1(0);
    if fDocumentStack=0 then
      raise EBSONException.CreateUTF8('Unexpected %.BSONDocumentEnd',[self]);
    dec(fDocumentStack);
    if fDocumentCount>=Length(fDocument) then
      SetLength(fDocument,NextGrow(fDocumentCount));
    with fDocument[fDocumentCount] do begin
      Offset := fDocumentStackOffset[fDocumentStack];
      Length := TotalWritten-Offset;
    end;
    inc(fDocumentCount);
    dec(CloseNumber);
  end;
end;

procedure TBSONWriter.BSONAdjustDocumentsSize(BSON: PByteArray);
var i: integer;
begin
  for i := 0 to fDocumentCount-1 do
  with fDocument[i] do
    PCardinal(@BSON[Offset])^ := Length;
end;

procedure TBSONWriter.ToBSONDocument(var result: TBSONDocument);
begin
  Flush;
  result := (Stream as TRawByteStringStream).DataString;
  BSONAdjustDocumentsSize(pointer(result));
end;

procedure TBSONWriter.ToBSONVariant(var result: variant; Kind: TBSONElementType);
var doc: TBSONDocument;
begin
  ToBSONDocument(doc);
  BSONVariantType.FromBSONDocument(doc,result,Kind);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: TVarRec);
var tmp: RawUTF8;
begin
  case value.VType of
    vtBoolean:  BSONWrite(name,value.VBoolean);
    vtInteger:  BSONWrite(name,value.VInteger);
    vtCurrency: BSONWrite(name,value.VCurrency^);
    vtExtended: BSONWrite(name,value.VExtended^);
    vtVariant:  BSONWriteVariant(name,value.VVariant^);
    vtInt64{$ifdef FPC},vtQWord{$endif}:
      BSONWrite(name,value.VInt64^);
    vtString, vtAnsiString, {$ifdef HASVARUSTRING}vtUnicodeString,{$endif}
    vtPChar, vtChar, vtWideChar, vtWideString: begin
      VarRecToUTF8(value,tmp);
      BSONWrite(name,tmp);
    end;
    else raise EBSONException.CreateUtf8(
      '%.BSONWrite(TVarRec.VType=%)',[self,value.VType]);
  end;
end;

procedure TBSONWriter.BSONWriteVariant(const name: RawUTF8; const value: variant);
  procedure WriteComplex;
  var temp: RawUTF8;
      JSON: PUTF8Char;
  begin
    with TVarData(value) do
    case VType of
    {$ifdef HASVARUSTRING}
    varUString: begin
      RawUnicodeToUtf8(VAny,length(UnicodeString(VAny)),temp);
      BSONWrite(Name,temp);
    end;
    {$endif}
    varOleStr: begin
      RawUnicodeToUtf8(VAny,length(WideString(VAny)),temp);
      BSONWrite(Name,temp);
    end;
    else begin
      VariantSaveJSON(value,twJSONEscape,temp);
      JSON := pointer(temp);
      BSONWriteFromJSON(name,JSON,nil);
      if JSON=nil then
        raise EBSONException.CreateUTF8('%.BSONWriteVariant(VType=%)',[self,VType]);
    end;
    end;
  end;
var dt: TDateTime;
begin
  with TVarData(value) do
    case VType of
    varEmpty,
    varNull:     BSONWrite(Name,betNull);
    varSmallint: BSONWrite(Name,VSmallInt);
    {$ifndef DELPHI5OROLDER}
    varShortInt: BSONWrite(Name,VShortInt);
    varWord:     BSONWrite(Name,VWord);
    varLongWord: BSONWrite(Name,VLongWord);
    {$endif}
    varByte:     BSONWrite(Name,VByte);
    varBoolean:  BSONWrite(Name,VBoolean);
    varInteger:  BSONWrite(Name,VInteger);
    varWord64,
    varInt64:    BSONWrite(Name,VInt64);
    varSingle:   BSONWrite(Name,VSingle);
    varDouble:   BSONWrite(Name,VDouble);
    varDate:     BSONWriteDateTime(Name,VDate);
    varCurrency: BSONWrite(Name,VCurrency);
    varString:
      if (VAny<>nil) and (PInteger(VAny)^ and $ffffff=JSON_SQLDATE_MAGIC) and
         Iso8601CheckAndDecode(PUTF8Char(VAny)+3,Length(RawUTF8(VAny))-3,dt) then
        // recognized TTextWriter.AddDateTime(woDateTimeWithMagic) ISO-8601 format
        BSONWriteDateTime(Name,dt) else
        BSONWrite(Name,RawUTF8(VAny)); // expect UTF-8 content
    else
    if VType=varByRef or varVariant then
      BSONWriteVariant(name,PVariant(VPointer)^) else
    if VType=BSONVariantType.VarType then
      BSONWrite(name,TBSONVariantData(value)) else
    if VType=DocVariantType.VarType then
      BSONWrite(name,TDocVariantData(value)) else
      WriteComplex;
    end;
end;

procedure TBSONWriter.BSONWriteDoc(const doc: TDocVariantData);
var Name: RawUTF8;
    i: PtrInt;
begin
  BSONDocumentBegin;
  if doc.VarType>varNull then // null,empty will write {}
    if doc.VarType<>DocVariantType.VarType then
      raise EBSONException.CreateUTF8('%.BSONWriteDoc(VType=%)',
        [self,doc.VarType]) else
    for i := 0 to doc.Count-1 do begin
      if doc.Names<>nil then
        Name := doc.Names[i] else
        UInt32ToUtf8(i,Name);
      BSONWriteVariant(Name,doc.Values[i]);
      if TotalWritten>BSON_MAXDOCUMENTSIZE then
        raise EBSONException.CreateUTF8('%.BSONWriteDoc(size=%>max %)',
          [self,TotalWritten,BSON_MAXDOCUMENTSIZE]);
    end;
  BSONDocumentEnd;
end;

procedure TBSONWriter.BSONWriteProjection(const FieldNamesCSV: RawUTF8);
var FieldNames: TRawUTF8DynArray;
    i: PtrInt;
begin
  CSVToRawUTF8DynArray(pointer(FieldNamesCSV),FieldNames);
  BSONDocumentBegin;
  for i := 0 to high(FieldNames) do
    BSONWrite(FieldNames[i],1);
  BSONDocumentEnd;
end;

function TBSONWriter.BSONWriteQueryOperator(name: RawUTF8; inverted: boolean;
  operator: TSynTableStatementOperator; const Value: variant): boolean;
const
  QUERY_OPS: array[opNotEqualTo..opIn] of RawUTF8 = (
    '$ne','$lt','$lte','$gt','$gte','$in');
  INVERT_OPS: array[opEqualTo..opGreaterThanOrEqualTo] of TSynTableStatementOperator = (
    opNotEqualTo,opEqualTo, opGreaterThanOrEqualTo,opGreaterThan,
    opLessThanOrEqualTo,opLessThan);
var wasString: boolean;
    like: RawUTF8;
    len: integer;
    doInvert: boolean;
begin
  result := false; // error on premature exit
  case Operator of
  // http://docs.mongodb.org/manual/faq/developers/#faq-developers-query-for-nulls
  // {$type:10} would return only existing fields, but our ODM do not insert
  // blobs by default -> do not use {$type:10} trick but plain {field:null}
  opIsNull:
    operator := opEqualTo;     // here Value=null
  opIsNotNull:
    operator := opNotEqualTo;  // here Value=null
  end;
  doInvert := false;
  if inverted then
    if operator<=high(INVERT_OPS) then
      operator := INVERT_OPS[operator] else begin
      doInvert := true;
      BSONDocumentBegin(name);
      name := '$not';
    end;
  case Operator of
  opEqualTo:
    BSONWriteVariant(name,Value);
  opNotEqualTo..opIn: begin
    BSONDocumentBegin(name);
    BSONWriteVariant(QUERY_OPS[operator],Value);
    BSONDocumentEnd;
  end;
  opLike: begin
    VariantToUTF8(Value,like,wasString);
    len := length(like);
    if (len=0) or not wasString then
      exit;
    if like[1]='%' then
      if len=1 then // LIKE '%' is invalid
        exit else
        if like[len]='%' then
          if len=2 then
            exit else // LIKE '%%' is invalid
            like := copy(like,2,len-2) else    // LIKE '%a%' -> /a/
          like := copy(like,2,len-1)+'$' else  // LIKE '%a'  -> /a$/
      if like[len]='%' then
        like := '^'+copy(like,1,len-1) else    // LIKE 'a%'  -> /^a/
        like := '^'+like+'$';                  // LIKE 'a'   -> /^a$/
    BSONWriteRegEx(name,like,'i'); // /like/i for case-insensitivity
  end;
  opContains: begin // http://docs.mongodb.org/manual/reference/operator/query/in
    BSONDocumentBegin(name);
    if _Safe(Value)^.Kind=dvArray then
      BSONWriteVariant(QUERY_OPS[opIn],Value) else begin
      BSONWrite(QUERY_OPS[opIn],betArray);
      BSONWriteArray([Value]);
    end;
    BSONDocumentEnd;
  end;
  else
    exit; // unhandled operator
  end;
  if doInvert then
    BSONDocumentEnd;
  result := true;
end;

procedure TBSONWriter.BSONWriteObject(const NameValuePairs: array of const);
var Name: RawUTF8;
    i: integer;
begin
  BSONDocumentBegin;
  for i := 0 to (length(NameValuePairs)shr 1)-1 do begin
    VarRecToUTF8(NameValuePairs[i*2],Name);
    BSONWrite(Name,NameValuePairs[i*2+1]);
  end;
  BSONDocumentEnd;
end;

procedure TBSONWriter.BSONWriteArray(const Items: array of const);
var i: integer;
begin
  BSONDocumentBegin;
  for i := 0 to high(Items) do
    BSONWrite(UInt32ToUtf8(i),Items[i]);
  BSONDocumentEnd;
end;

procedure TBSONWriter.BSONWriteArrayOfInteger(const Integers: array of integer);
var i: integer;
begin
  BSONDocumentBegin;
  for i := 0 to high(Integers) do
    BSONWrite(UInt32ToUtf8(i),Integers[i]);
  BSONDocumentEnd;
end;

procedure TBSONWriter.BSONWriteArrayOfInt64(const Integers: array of Int64);
var i: integer;
begin
  BSONDocumentBegin;
  for i := 0 to high(Integers) do
    BSONWrite(UInt32ToUtf8(i),Integers[i]);
  BSONDocumentEnd;
end;

procedure TBSONWriter.BSONWriteFromJSON(const name: RawUTF8; var JSON: PUTF8Char;
  EndOfObject: PUTF8Char; DoNotTryExtendedMongoSyntax: boolean);
var tmp: variant;
    blob: RawByteString;
    wasString: boolean;
    Value: PUTF8Char;
    ValueLen: integer;
    VDouble: double;
    ValueDateTime: TDateTime absolute VDouble;
    VInt64: Int64 absolute VDouble;
    Kind: TBSONElementType;
begin
  if JSON^ in [#1..' '] then repeat inc(JSON) until not(JSON^ in [#1..' ']);
  if not DoNotTryExtendedMongoSyntax and
     BSONVariantType.TryJSONToVariant(JSON,tmp,EndOfObject) then
    // was betDateTime, betObjectID or betRegEx, from strict or extended JSON
    BSONWriteVariant(name,tmp) else
    // try from simple types
    case JSON^ of
    #0: begin
      JSON := nil;
      exit;
    end;
    '[': begin // nested array
      BSONWrite(name,betArray);
      JSON := BSONWriteDocFromJSON(JSON,EndOfObject,Kind,DoNotTryExtendedMongoSyntax);
    end;
    '{': begin // nested document
      BSONWrite(name,betDoc);
      JSON := BSONWriteDocFromJSON(JSON,EndOfObject,Kind,DoNotTryExtendedMongoSyntax);
    end;
    else begin // simple types
      Value := GetJSONField(JSON,JSON,@wasString,EndOfObject,@ValueLen);
      if JSON=nil then
        JSON := @NULCHAR;
      if (Value=nil) or not wasString then
        if GetVariantFromNotStringJSON(Value,TVarData(tmp),true) then begin
          BSONWriteVariant(name,tmp); // null,boolean,Int64,double
          exit;
        end;
      // found no simple value -> check text value
      if Base64MagicCheckAndDecode(Value,ValueLen,blob) then
        // recognized '\uFFF0base64encodedbinary' pattern
        BSONWrite(name,pointer(blob),length(blob)) else
      if Iso8601CheckAndDecode(Value,ValueLen,ValueDateTime) then
        // recognized TTextWriter.AddDateTime() pattern
        BSONWriteDateTime(name,ValueDateTime) else
      if (PInteger(Value)^ and $ffffff=JSON_SQLDATE_MAGIC) and
         Iso8601CheckAndDecode(Value+3,ValueLen-3,ValueDateTime) then
        // recognized TTextWriter.AddDateTime(woDateTimeWithMagic) pattern
        BSONWriteDateTime(name,ValueDateTime) else
        // will point to the in-place escaped JSON text
        BSONWriteString(name,Value,ValueLen);
    end;
  end;
  if TotalWritten>BSON_MAXDOCUMENTSIZE then
    raise EBSONException.CreateUTF8('%.BSONWriteDoc(size=% > max=%)',
      [self,TotalWritten,BSON_MAXDOCUMENTSIZE]);
end;

function TBSONWriter.BSONWriteDocFromJSON(JSON: PUTF8Char; aEndOfObject: PUTF8Char;
  out Kind: TBSONElementType; DoNotTryExtendedMongoSyntax: boolean): PUTF8Char;
var ndx: cardinal;
    EndOfObject: AnsiChar;
    Name: RawUTF8;
begin
  result := nil;
  if JSON=nil then
    exit;
  if JSON^ in [#1..' '] then repeat inc(JSON) until not(JSON^ in [#1..' ']);
  case JSON^ of
  '[': begin
    Kind := betArray;
    BSONDocumentBegin;
    repeat inc(JSON) until not(JSON^ in [#1..' ']);
    ndx := 0;
    if JSON^=']' then
      inc(JSON) else
      repeat
        UInt32ToUtf8(ndx,Name);
        BSONWriteFromJSON(Name,JSON,@EndOfObject,DoNotTryExtendedMongoSyntax);
        if JSON=nil then
          exit; // invalid content
        inc(ndx);
      until EndOfObject=']';
  end;
  '{': begin
    Kind := betDoc;
    BSONDocumentBegin;
    repeat inc(JSON) until not(JSON^ in [#1..' ']);
    if JSON^='}' then
      inc(JSON) else
      repeat
        // see http://docs.mongodb.org/manual/reference/mongodb-extended-json
        Name := GetJSONPropName(JSON); // BSON/JSON accepts "" as key name
        BSONWriteFromJSON(Name,JSON,@EndOfObject,DoNotTryExtendedMongoSyntax);
        if (JSON=nil) or (EndOfObject=#0) then
          exit; // invalid content
      until EndOfObject='}';
  end;
  'n','N':
    if IdemPChar(JSON+1,'ULL') then begin // append null as {}
      Kind := betDoc;
      BSONDocumentBegin;
      inc(JSON,4);
    end else
      exit;
  else exit;
  end;
  BSONDocumentEnd;
  if JSON^ in [#1..' '] then repeat inc(JSON) until not(JSON^ in [#1..' ']);
  if aEndOfObject<>nil then
    aEndOfObject^ := JSON^;
  if JSON^<>#0 then
    repeat inc(JSON) until not(JSON^ in [#1..' ']);
  result := JSON; // indicates successfully parsed
end;


{ TBSONObjectID }

var
  GlobalBSONObjectID: record
    Section: TRTLCriticalSection;
    Default: packed record
      Counter: cardinal;
      MachineID: TBSON24;
      ProcessID: word;
    end;
    LastCreateTime: cardinal;
    LastCounter: cardinal;
  end;

procedure InitBSONObjectIDComputeNew;
begin
  InitializeCriticalSection(GlobalBSONObjectID.Section);
  with GlobalBSONObjectID.Default do begin
    TAESPRNG.Main.FillRandom(@Counter,3);
    with ExeVersion do
      PCardinal(@MachineID)^ := crc32c(crc32c(0,pointer(Host),length(Host)),
        pointer(User),length(User));
    ProcessID := crc32c(0,@MainThreadID,SizeOf(MainThreadID)); // lower 16-bit
  end;
end;

procedure TBSONObjectID.Init;
begin // 12 bytes fill zero
  with PHash128Rec(@self)^ do begin
    i0 := 0;
    i1 := 0;
    i2 := 0;
  end;
end;

procedure TBSONObjectID.ComputeNew;
var now, count: cardinal;
begin
  now := UnixTimeUTC; // fast API call (no need of cache)
  with GlobalBSONObjectID do begin
    EnterCriticalSection(Section);
    if now>LastCreateTime then begin
      LastCreateTime := now;
      count := Default.Counter; // reset
    end else begin
      count := LastCounter+1;
      if count and $ffffff=Default.Counter then begin
        count := Default.Counter; // collision -> cheat on timestamp
        inc(LastCreateTime);
      end;
    end;
    Counter.b1 := count shr 16; // stored as bigendian
    Counter.b2 := count shr 8;
    Counter.b3 := count;
    LastCounter := count;
    UnixCreateTime := bswap32(LastCreateTime);
    MachineID := Default.MachineID;
    ProcessID := Default.ProcessID;
    LeaveCriticalSection(Section);
  end;
end;

function TBSONObjectID.Equal(const Another: TBSONObjectID): boolean;
begin // first check Counter last field, which is more likely to diverse
  result := (PIntegerArray(@Self)[2]=PIntegerArray(@Another)[2]) and
    {$ifdef CPU64}
    (PInt64(@Self)^=PInt64(@Another)^);
    {$else}
    (PIntegerArray(@Self)[1]=PIntegerArray(@Another)[1]) and
    (PIntegerArray(@Self)[0]=PIntegerArray(@Another)[0]);
    {$endif}
end;

function TBSONObjectID.Equal(const Another: variant): boolean;
var oid2: TBSONObjectID;
begin
  result := oid2.FromVariant(Another) and Equal(oid2);
end;

function TBSONObjectID.CreateDateTime: TDateTime;
begin
  result := UnixTimeToDateTime(bswap32(UnixCreateTime));
end;

function TBSONObjectID.ToText: RawUTF8;
begin
  ToText(result);
end;

function TBSONObjectID.ToVariant: variant;
begin
  VarClear(result);
  with TBSONVariantData(result) do begin
    VType := BSONVariantType.VarType;
    VKind := betObjectID;
    VObjectID := self;
  end;
end;

procedure TBSONObjectID.ToVariant(var result: variant);
begin
  VarClear(result);
  with TBSONVariantData(result) do begin
    VType := BSONVariantType.VarType;
    VKind := betObjectID;
    VObjectID := self;
  end;
end;

function TBSONObjectID.FromText(const Text: RawUTF8): boolean;
begin
  if length(Text)=SizeOf(self)*2 then
    result := SynCommons.HexToBin(Pointer(Text),@self,SizeOf(self)) else
    result := false;
end;

function TBSONObjectID.FromText(Text: PUTF8Char): boolean;
begin
  result := SynCommons.HexToBin(Pointer(Text),@self,SizeOf(self));
end;

function TBSONObjectID.FromVariant(const value: variant): boolean;
var txt: RawUTF8;
    wasString: boolean;
    bson: PBSONVariantData;
begin
  bson := @value;
  if bson^.VType=varByRef or varVariant then
    bson := TVarData(value).VPointer;
  if (bson^.VType=BSONVariantType.VarType) and (bson^.VKind=betObjectID) then begin
    self := bson^.VObjectID;
    result := true;
  end else begin
    VariantToUTF8(value,txt,wasString);
    result := wasString and FromText(txt);
  end;
end;

procedure TBSONObjectID.ToText(var result: RawUTF8);
begin
  FastSetString(result,nil,sizeof(self)*2);
  SynCommons.BinToHex(@self,pointer(result),sizeof(self));
end;


{ TBSONVariant }

procedure TBSONVariant.ToJSON(W: TTextWriter; const Value: variant; Escape: TTextWriterKind);
var item: TBSONElement;
    temp: RawByteString;
begin
  item.FromVariant('',Value,temp);
  item.AddMongoJSON(W,modMongoStrict);
end;

function TBSONVariant.GetNewDoc(const BSONDoc: TBSONDocument): variant;
begin
  FromBSONDocument(BSONDoc,result);
end;

function TBSONVariant.IsOfKind(const V: variant;
  Kind: TBSONElementType): boolean;
begin
  with TBSONVariantData(V) do
    if VType=varByRef or varVariant then
      result := IsOfKind(PVariant(TVarData(V).VPointer)^,Kind) else
      result := (self<>nil) and (VType=VarType) and (VKind=Kind);
end;

function TBSONVariant.ToBlob(const V: Variant; var Blob: RawByteString): boolean;
begin
  with TVarData(V) do
    if VType=varByRef or varVariant then begin
      result := ToBlob(PVariant(VPointer)^,Blob);
      exit;
     end;
  with TBSONVariantData(V) do begin
    result := (VType=VarType) and (VKind=betBinary);
    if result then
      if (VBlob=nil) or
         (PInteger(VBlob)^<>Length(RawByteString(VBlob))-(sizeof(integer)+1)) then
        Blob := '' else
        SetString(Blob,PAnsiChar(VBlob)+(sizeof(integer)+1),PInteger(VBlob)^);
  end;
end;

procedure TBSONVariant.FromBinary(const Bin: RawByteString;
  BinType: TBSONElementBinaryType; var result: variant);
var Len: integer;
begin // "\x05" e_name int32 subtype (byte*)
  VarClear(result);
  with TBSONVariantData(result) do begin
    if Bin='' then begin
      VType := varNull; // stores a NULL
      exit;
    end;
    VType := VarType;
    VKind := betBinary;
    VBlob := nil; // avoid GPF here below
    Len := length(Bin);
    SetLength(RawByteString(VBlob),Len+(sizeof(integer)+1));
    PInteger(VBlob)^ := Len;
    PByteArray(VBlob)^[sizeof(integer)] := ord(BinType);
    MoveFast(pointer(Bin)^,PByteArray(VBlob)^[sizeof(integer)+1],Len);
  end;
end;

procedure TBSONVariant.FromBSONDocument(const BSONDoc: TBSONDocument;
  var result: variant; Kind: TBSONElementType);
begin
  VarClear(result);
  with TBSONVariantData(result) do begin
    VType := VarType;
    VKind := Kind;
    VBlob := nil; // avoid GPF here below
    RawByteString(VBlob) := BSONDoc;
  end;
end;

procedure TBSONVariant.FromJSON(json: PUTF8Char; var result: variant);
begin
  VarClear(result);
  if json=nil then
    exit;
  if json^ in [#1..' '] then repeat inc(json) until not(json^ in [#1..' ']);
  if json^ in ['{','['] then
    with TBSONVariantData(result) do begin
      VType := VarType;
      VBlob := nil; // avoid GPF here below
      VKind := JSONBufferToBSONDocument(json,TBSONDocument(VBlob));
    end else
      VariantLoadJSON(result,json);
end;

const
  BSON_JSON_NEWDATE: string[8] = 'ew Date('; // circumvent Delphi XE4 Win64 bug

{$IFDEF FPC} {$PUSH} {$ENDIF} {$HINTS OFF} // avoid hints with CompareMemFixed() inlining
function TBSONVariant.TryJSONToVariant(var JSON: PUTF8Char;
  var Value: variant; EndOfObject: PUTF8Char): boolean;
// warning: code should NOT modify JSON buffer in-place, unless it returns true
var bsonvalue: TBSONVariantData absolute Value;
    varvalue: TVarData absolute Value;
  procedure Return(kind: TBSONElementType; P: PUTF8Char; GotoEndOfObject: AnsiChar);
  begin
    if GotoEndOfObject<>#0 then
      while P^<>GotoEndOfObject do
      if P^=#0 then begin
        if kind in [betRegEx,betDecimal128] then
          RawByteString(bsonvalue.VBlob) := ''; // avoid memory leak
        exit;
      end else inc(P);
    P := GotoNextNotSpace(P+1);
    if EndOfObject<>nil then
      EndOfObject^ := P^;
    if P^<>#0 then
      JSON := P+1 else
      JSON := P;
    case kind of
    betObjectID, betRegEx, betDecimal128: begin // see TBSONWriter.BSONWrite()
      bsonvalue.VType := VarType;
      bsonvalue.VKind := kind;
    end;
    betDateTime:
      varvalue.VType := varDate;
    end;
    result := true;
  end;
  procedure ReturnInt(kindint: integer; P: PUTF8Char; GotoEndOfObject: AnsiChar);
  {$ifdef HASINLINE}inline;{$endif} // redirection function to circumvent FPC trunk limitation
  var kind: TBSONElementType absolute kindint;
  begin
    Return(kind,P,GotoEndOfObject);
  end;
  procedure TryDate(P: PUTF8Char; GotoEndOfObject: AnsiChar);
  var L: integer;
  begin
    P := GotoNextNotSpace(P);
    if GotoEndOfObject=')' then
      if (P^=')') then begin // new date() constructor
        varvalue.VDate := NowUTC;
        Return(betDateTime,P,#0);
        exit;
      end else
      if P^ in ['0'..'9'] then begin
        varvalue.VDate := GetNextItemDouble(P,')');
        if (varvalue.VDate<>0) and (P<>nil) then begin
          Return(betDateTime,P-1,#0);
          exit;
        end;
      end;
    if P^<>'"' then exit;
    if PCardinal(P)^=JSON_SQLDATE_MAGIC_QUOTE then
      inc(P,3); // ignore\uFFF1 code for DateTimeToSQL/TimeLogToSQL functions
    L := 1; while P[L]<>'"' do if P[L]<=' ' then exit else inc(L);
    Iso8601ToDateTimePUTF8CharVar(P+1,L,varvalue.VDate);
    if varvalue.VDate<>0 then
      Return(betDateTime,P+L+1,GotoEndOfObject);
  end;
  procedure TryObjectID(P: PUTF8Char; GotoEndOfObject: AnsiChar);
  begin
    P := GotoNextNotSpace(P);
    if (GotoEndOfObject=')') and (P^=')') then begin // ObjectId() constructor
      bsonvalue.VObjectID.ComputeNew;
      Return(betObjectID,P,#0);
      exit;
    end;
    if P^<>'"' then exit;
    if bsonvalue.VObjectID.FromText(P+1) then
      Return(betObjectID,P+25,GotoEndOfObject);
  end;
  procedure TryDecimal(P: PUTF8Char; GotoEndOfObject: AnsiChar);
  var dec: TDecimal128;
      L: integer;
  begin
    if P^<>'"' then exit;
    inc(P);
    L := 0;
    while P[L]<>'"' do
      if not(P[L] in ['0'..'9','e','E','+','-','.']) then exit else inc(L);
    if dec.FromText(P,L)=dsvError then
      exit;
    bsonvalue.VBlob := nil; // avoid GPF
    SetString(RawByteString(bsonvalue.VBlob),PAnsiChar(@dec),sizeof(TDecimal128));
    Return(betDecimal128,P+L+1,GotoEndOfObject);
  end;
  var Reg,Opt: PUTF8Char;
      RegLen,OptLen: integer;
  procedure ReturnRegEx(P: PUTF8Char; GotoEndOfObject: AnsiChar);
  var buf: PAnsiChar;
  begin
    bsonvalue.VBlob := nil; // avoid GPF
    SetString(RawByteString(bsonvalue.VBlob),nil,RegLen+OptLen+2);
    buf := bsonvalue.VBlob;
    MoveFast(Reg^,buf^,RegLen); inc(buf,RegLen); buf^ := #0; inc(buf);
    MoveFast(Opt^,buf^,OptLen); inc(buf,OptLen); buf^ := #0;
    Return(betRegEx,P,GotoEndOfObject);
  end;
  procedure TryRegExShell(P: PUTF8Char);
  begin
    RegLen := 0; while P[RegLen]<>'/' do if P[RegLen]<=' ' then exit else inc(RegLen);
    Reg := P;
    inc(P,RegLen);
    if P^<>'/' then exit else inc(P);
    OptLen := 0; while tcWord in TEXT_CHARS[P[OptLen]] do inc(OptLen);
    if P[OptLen]=#0 then exit;
    Opt := P;
    ReturnRegEx(Opt+OptLen-1,#0);
  end;
  procedure TryRegExStrict(P: PUTF8Char);
  begin // warning: this won't escape double quotes...
    P := GotoNextNotSpace(P);
    if P^<>'"' then exit else inc(P);
    RegLen := 0; while P[RegLen]<>'"' do if P[RegLen]<=' ' then exit else inc(RegLen);
    Reg := P;
    P := GotoNextNotSpace(Reg+RegLen+1);
    if P^<>',' then Exit; // $regex:"acme*.corp",$options:"i"}
    P := GotoNextNotSpace(P+1);
    if P^='"' then inc(P);
    if PInt64(P)^<>PInt64(@BSON_JSON_REGEX[1][4])^ then exit else inc(P,8);
    if P^='"' then inc(P);
    P := GotoNextNotSpace(P);
    if P^<>':' then exit;
    P := GotoNextNotSpace(P+1);
    if P^<>'"' then exit else inc(P);
    OptLen := 0; while P[OptLen]<>'"' do if P[OptLen]<=' ' then exit else inc(OptLen);
    Opt := P;
    ReturnRegEx(Opt+OptLen+1,'}');
  end;
var P: PUTF8Char;
begin // here JSON does not start with " or 1..9 (obvious simple types)
  // see http://docs.mongodb.org/manual/reference/mongodb-extended-json
  result := false;
  case NormToUpperAnsi7[JSON^] of
  '{': begin // strict MongoDB objects e.g. {"$undefined":true} or {"$oid":".."}
    P := JSON;
    repeat inc(P) until not(P^ in [#1..' ']);
    if P^<>'"' then exit;
    repeat inc(P) until not(P^ in [#1..' ']);
    if P[0]='$' then
    case P[1] of
    'u': if CompareMemFixed(P+2,@BSON_JSON_UNDEFINED[false][5],10) then
           Return(betDeprecatedUndefined,P+12,'}');
    'm': if CompareMemFixed(P+1,@BSON_JSON_MINKEY[false][4],8) then
           ReturnInt(betMinKey,P+9,'}') else
         if CompareMemFixed(P+1,@BSON_JSON_MAXKEY[false][4],8) then
           ReturnInt(betMaxKey,P+9,'}');
    'o': if PInteger(P+2)^=PInteger(@BSON_JSON_OBJECTID[false,modMongoStrict][5])^ then
           TryObjectID(P+6,'}');
    'd': if CompareMemSmall(P+2,@BSON_JSON_DATE[modMongoStrict,false][5],5) then
           TryDate(P+7,'}');
    'r': if CompareMemFixed(P,@BSON_JSON_REGEX[0][3],8) then
           TryRegExStrict(P+8);
    'n': if CompareMemFixed(P,@BSON_JSON_DECIMAL[false,modMongoStrict][3],16) then
           TryDecimal(P+16,'}');
    end;
  end;
  // MongoDB Shell Mode extended syntax
  'U': if StrCompIL(JSON+1,@BSON_JSON_UNDEFINED[true][2],8)=0 then
         Return(betDeprecatedUndefined,JSON+8,#0);
  'M': if StrCompIL(JSON+1,@BSON_JSON_MINKEY[true][2],5)=0 then
         ReturnInt(betMinKey,JSON+5,#0) else
       if StrCompIL(JSON+1,@BSON_JSON_MAXKEY[true][2],7)=0 then
         ReturnInt(betMaxKey,JSON+5,#0);
  'O': if StrCompIL(JSON+1,@BSON_JSON_OBJECTID[false,modMongoShell][2],8)=0 then
         TryObjectID(JSON+9,')');
  'N': if StrCompIL(JSON+1,@BSON_JSON_NEWDATE[1],8)=0 then
          TryDate(JSON+9,')') else
       if StrCompIL(JSON+1,@BSON_JSON_DECIMAL[false,modMongoShell][2],13)=0 then
          TryDecimal(JSON+14,')');
  'I': if StrCompIL(JSON+1,@BSON_JSON_DATE[modMongoShell,false][2],7)=0 then
          TryDate(JSON+8,')');
  '/': TryRegExShell(JSON+1);
  end;
end;
{$IFDEF FPC} {$POP} {$ELSE} {$HINTS ON} {$ENDIF}

procedure TBSONVariant.Cast(var Dest: TVarData; const Source: TVarData);
begin
  CastTo(Dest,Source,VarType);
end;

procedure TBSONVariant.CastTo(var Dest: TVarData;
  const Source: TVarData; const AVarType: TVarType);
var tmp: RawUTF8;
    wasString: boolean;
begin
  if AVarType=VarType then begin
    VariantToUTF8(Variant(Source),tmp,wasString);
    if wasString then begin
      VarClear(variant(Dest));
      if TBSONVariantData(Dest).VObjectID.FromText(tmp) then begin
        Dest.VType := VarType;
        TBSONVariantData(Dest).VKind := betObjectID;
        exit;
      end;
      variant(Dest) := BSONVariant(tmp); // convert from JSON text
      exit;
    end;
    RaiseCastError;
  end else begin
    if Source.VType<>VarType then
      RaiseCastError;
    with TBSONVariantData(Source) do
      if (VKind=betObjectID) and (AVarType in [varDate,varDouble]) then begin
        Dest.VType := AVarType;
        Dest.VDate := VObjectID.CreateDateTime;
        exit;
      end else begin
        if VKind=betObjectID then
          VObjectID.ToText(tmp) else
          tmp := VariantSaveMongoJSON(variant(Source),modMongoShell);
        RawUTF8ToVariant(tmp,Dest,AVarType); // convert to JSON text
      end;
  end;
end;

procedure TBSONVariant.Clear(var V: TVarData);
begin
  if TBSONVariantData(V).VKind in BSON_ELEMENTVARIANTMANAGED then
    RawByteString(TBSONVariantData(V).VBlob) := '';
  ZeroFill(@V); // will set V.VType := varEmpty
end;

procedure TBSONVariant.Copy(var Dest: TVarData;
  const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect then
    SimplisticCopy(Dest,Source,true) else begin
    VarClear(variant(Dest)); // Dest may be a complex type
    Dest := Source;
    with TBSONVariantData(Dest) do
    if VKind in BSON_ELEMENTVARIANTMANAGED then begin
      VBlob := nil; // avoid GPF
      RawByteString(VBlob) := RawByteString(TBSONVariantData(Source).VBlob);
    end;
  end;
end;

procedure TBSONVariant.Compare(const Left, Right: TVarData;
  var Relationship: TVarCompareResult);
var res: integer;
    LeftU,RightU: RawUTF8;
begin
  LeftU := VariantSaveMongoJSON(variant(Left),modMongoStrict);
  RightU := VariantSaveMongoJSON(variant(Right),modMongoStrict);
  if LeftU=RightU then
    Relationship := crEqual else begin
    res := StrComp(pointer(LeftU),pointer(RightU));
    if res<0 then
      Relationship := crLessThan else
    if res>0 then
      Relationship := crGreaterThan else
      Relationship := crEqual;
  end;
end;


{ main BSON* functions }

function ToText(kind: TBSONElementType): PShortString;
begin
  result := GetEnumName(TypeInfo(TBSONElementType),ord(kind));
end;

function ToText(spec: TDecimal128SpecialValue): PShortString;
begin
  result := GetEnumName(TypeInfo(TDecimal128SpecialValue),ord(spec));
end;

function ToText(op: TMongoOperation): PShortString;
begin
  result := GetEnumName(TypeInfo(TMongoOperation),ord(op));
end;

function ToText(wc: TMongoClientWriteConcern): PShortString;
begin
  result := GetEnumName(TypeInfo(TMongoClientWriteConcern),ord(wc));
end;

function ToText(pref: TMongoClientReplicaSetReadPreference): PShortString;
begin
  result := GetEnumName(TypeInfo(TMongoClientReplicaSetReadPreference),ord(pref));
end;


function ObjectID: variant;
var ID: TBSONObjectID;
begin
  ID.ComputeNew;
  ID.ToVariant(result);
end;

function ObjectID(const Hexa: RawUTF8): variant;
var ID: TBSONObjectID;
begin
  if ID.FromText(Hexa) then
    ID.ToVariant(result) else
    raise EBSONException.CreateUTF8('Invalid ObjectID("%")',[Hexa]);
end;

function BSONObjectID(const aObjectID: variant): TBSONObjectID;
begin
  if not result.FromVariant(aObjectID) then
    raise EBSONException.Create('BSONObjectID() over not ObjectID variant');
end;

function JavaScript(const JS: RawUTF8): variant;
begin
  VarClear(result);
  with TBSONVariantData(result) do begin
    VType := BSONVariantType.VarType;
    VKind := betJS;
    VText := nil; // avoid GPF
    RawUTF8(VText) := JS;
  end;
end;

function JavaScript(const JS: RawUTF8; const Scope: TBSONDocument): variant;
var Len, JSLen: integer;
begin
  VarClear(result);
  with TBSONVariantData(result) do begin
    VType := BSONVariantType.VarType;
    VKind := betJSScope;
    JSLen := Length(JS)+1;                        // string = int32 text#0
    Len := SizeOf(integer)*2+JSLen+length(Scope); // int32 string document
    VBlob := nil; // avoid GPF
    SetLength(RawByteString(VBlob),Len);
    PIntegerArray(VBlob)^[0] := Len;              // length:int32
    PIntegerArray(VBlob)^[1] := JSLen;            // string:int32
    MoveFast(pointer(JS)^,PAnsiChar(VBlob)[8],JSLen); // string:text#0
    MoveFast(pointer(Scope)^,PAnsiChar(VBlob)[8+JSLen],Length(Scope)); // document
  end;
end;

function NumberDecimal(const Value: RawUTF8): variant;
var dec: TDecimal128;
begin
  if dec.FromText(Value)=dsvError then
    raise EBSONException.CreateUTF8('Invalid NumberDecimal("%")',[Value]);
  dec.ToVariant(result);
end;

function NumberDecimal(const Value: currency): variant;
var dec: TDecimal128;
begin
  dec.FromCurr(Value);
  dec.ToVariant(result);
end;

function BSON(const doc: TDocVariantData): TBSONDocument;
begin
  if doc.VarType=varVariant or varByRef then begin
    result := BSON(PDocVariantData(TVarData(doc).VPointer)^);
    exit;
  end;
  if doc.VarType<>DocVariantType.VarType then
    raise EBSONException.Create('doc is not a TDocVariant');
  with TBSONWriter.Create(TRawByteStringStream) do
  try
    BSONWriteDoc(doc);
    ToBSONDocument(result);
  finally
    Free;
  end;
end;

function BSONFromIntegers(const Integers: array of integer): TBSONDocument;
begin
  with TBSONWriter.Create(TRawByteStringStream) do
  try
    BSONWriteArrayOfInteger(Integers);
    ToBSONDocument(result);
  finally
    Free;
  end;
end;

function BSONFromInt64s(const Integers: array of Int64): TBSONDocument;
begin
  with TBSONWriter.Create(TRawByteStringStream) do
  try
    BSONWriteArrayOfInt64(Integers);
    ToBSONDocument(result);
  finally
    Free;
  end;
end;

function BSON(const NameValuePairs: array of const): TBSONDocument;
var W: TBSONWriter;
    name: RawUTF8;
    a: integer;
procedure WriteValue;
var ndx: cardinal;
begin
  case VarRecAsChar(NameValuePairs[a]) of
  ord('['): begin
    W.BSONDocumentBegin(name,betArray);
    ndx := 0;
    repeat
      inc(a);
      if VarRecAsChar(NameValuePairs[a])=ord(']') then
        break;
      UInt32ToUtf8(ndx,name);
      WriteValue;
      inc(ndx);
    until a=high(NameValuePairs);
    W.BSONDocumentEnd;
  end;
  ord('{'): begin
    W.BSONDocumentBegin(name,betDoc);
    repeat
      inc(a);
      VarRecToUTF8(NameValuePairs[a],name);
      if (a=high(NameValuePairs)) or (name='}') then
        break;
      inc(a);
      WriteValue;
    until a=high(NameValuePairs);
    W.BSONDocumentEnd;
  end else
    W.BSONWrite(name,NameValuePairs[a]);
  end;
end;
begin
  W := TBSONWriter.Create(TRawByteStringStream);
  try
    W.BSONDocumentBegin;
    a := 0;
    while a<high(NameValuePairs) do begin
      VarRecToUTF8(NameValuePairs[a],name);
      inc(a);
      WriteValue;
      inc(a);
    end;
    W.BSONDocumentEnd;
    W.ToBSONDocument(result);
  finally
    W.Free;
  end;
end;

function BSONFieldSelector(const FieldNames: array of RawUTF8): TBSONDocument;
var i: integer;
    W: TBSONWriter;
begin
  W := TBSONWriter.Create(TRawByteStringStream,512);
  try
    W.BSONDocumentBegin;
    for i := 0 to high(FieldNames) do
      W.BSONWrite(FieldNames[i],1);
    W.BSONDocumentEnd;
    W.ToBSONDocument(result);
  finally
    W.Free;
  end;
end;

function BSONFieldSelector(const FieldNamesCSV: RawUTF8): TBSONDocument;
var FieldNames: TRawUTF8DynArray;
begin
  CSVToRawUTF8DynArray(pointer(FieldNamesCSV),FieldNames);
  result := BSONFieldSelector(FieldNames);
end;

function JSONBufferToBSONDocument(JSON: PUTF8Char; var doc: TBSONDocument;
  DoNotTryExtendedMongoSyntax: boolean): TBSONElementType;
var W: TBSONWriter;
begin
  W := TBSONWriter.Create(TRawByteStringStream);
  try
    W.BSONWriteDocFromJSON(JSON,nil,result,DoNotTryExtendedMongoSyntax);
    W.ToBSONDocument(doc);
  finally
    W.Free;
  end;
end;

function JSONBufferToBSONArray(JSON: PUTF8Char; out docs: TBSONDocumentDynArray;
  DoNotTryExtendedMongoSyntax: boolean): boolean;
var W: TBSONWriter;
    doc: TBSONDocument;
    EndOfObject: AnsiChar;
    Kind: TBSONElementType;
    n: integer;
begin
  result := false;
  if JSON=nil then
    exit;
  JSON := GotoNextNotSpace(JSON);
  if JSON^<>'[' then
    exit;
  JSON := GotoNextNotSpace(JSON+1);
  n := 0;
  W := TBSONWriter.Create(TRawByteStringStream,16384);
  try
    repeat
      JSON := W.BSONWriteDocFromJSON(JSON,@EndOfObject,Kind,DoNotTryExtendedMongoSyntax);
      if JSON=nil then
        exit;
      W.ToBSONDocument(doc);
      if n>=length(docs) then
        SetLength(docs,NextGrow(n));
      docs[n] := doc;
      inc(n);
      W.CancelAll;
    until EndOfObject=']';
  finally
    W.Free;
  end;
  SetLength(docs,n);
  result := true;
end;

function BSON(const Format: RawUTF8; const Args,Params: array of const;
  kind: PBSONElementType): TBSONDocument;
var JSON: RawUTF8; // since we use FormatUTF8(), TSynTempBuffer is useless here
    v: variant;
    k: TBSONElementType;
begin
  if (Format='?') and (high(Params)>=0) then begin
    VarRecToVariant(Params[0],v);
    if DocVariantType.IsOfType(v) then begin
      result := BSON(TDocVariantData(v));
      if kind<>nil then
        if TDocVariantData(v).Kind=dvArray then
          kind^ := betArray else
          kind^ := betDoc;
      exit;
    end;
  end;
  JSON := FormatUTF8(Format,Args,Params,true);
  UniqueRawUTF8(JSON); // ensure Format is untouched if Args=[]
  k := JSONBufferToBSONDocument(pointer(JSON),result);
  if kind<>nil then
    kind^ := k;
end;

function BSON(const JSON: RawUTF8; kind: PBSONElementType): TBSONDocument;
var tmp: TSynTempBuffer;
    k: TBSONElementType;
begin
  tmp.Init(JSON);
  try
    k := JSONBufferToBSONDocument(tmp.buf,result);
    if kind<>nil then
      kind^ := k;
  finally
    tmp.Done;
  end;
end;

function BSONVariant(const NameValuePairs: array of const): variant;
begin
  BSONVariantType.FromBSONDocument(BSON(NameValuePairs),result,betDoc);
end;

function BSONVariant(const JSON: RawUTF8): variant;
var k: TBSONElementType;
    b: RawByteString;
begin
  b := BSON(JSON,@k);
  BSONVariantType.FromBSONDocument(b,result,k);
end;

procedure BSONVariant(JSON: PUTF8Char; var result: variant);
var tmp: TBSONDocument;
begin
  JSONBufferToBSONDocument(JSON,tmp);
  BSONVariantType.FromBSONDocument(tmp,result);
end;

function BSONVariant(const Format: RawUTF8; const Args,Params: array of const): variant; overload;
var k: TBSONElementType;
    b: RawByteString;
begin
  b := BSON(Format,Args,Params,@k);
  BSONVariantType.FromBSONDocument(b,result,k);
end;

function BSONVariant(doc: TDocVariantData): variant; overload;
var k: TBSONElementType;
begin
  if doc.Kind=dvArray then
    k := betArray else
    k := betDoc;
  BSONVariantType.FromBSONDocument(BSON(Doc),result,k);
end;

function BSONVariantFieldSelector(const FieldNames: array of RawUTF8): variant;
begin
  BSONVariantType.FromBSONDocument(BSONFieldSelector(FieldNames),result);
end;

function BSONVariantFieldSelector(const FieldNamesCSV: RawUTF8): variant;
begin
  BSONVariantType.FromBSONDocument(BSONFieldSelector(FieldNamesCSV),result);
end;

function BSONVariantFromIntegers(const Integers: array of integer): variant;
begin
  BSONVariantType.FromBSONDocument(BSONFromIntegers(Integers),result,betArray);
end;

function BSONVariantFromInt64s(const Integers: array of Int64): variant;
begin
  BSONVariantType.FromBSONDocument(BSONFromInt64s(Integers),result,betArray);
end;


{ ************ MongoDB client }

{ TMongoRequest }

const
  WIRE_OPCODES: array[TMongoOperation] of integer = (
   1, 1000, 2001, 2002, 2004, 2005, 2006, 2007, 2013);
  CLIENT_OPCODES = [opUpdate,opInsert,opQuery,opGetMore,opDelete,opKillCursors];

var
  GlobalRequestID: integer;

constructor TMongoRequest.Create(const FullCollectionName: RawUTF8;
  opCode: TMongoOperation; requestID, responseTo: integer);
begin
  if not (opCode in CLIENT_OPCODES) then
    raise EMongoException.CreateUTF8('Unexpected %.Create(opCode=%)',[self,ToText(opCode)^]);
  inherited Create(TRawByteStringStream);
  fFullCollectionName := FullCollectionName;
  Split(fFullCollectionName,'.',fDatabaseName,fCollectionName);
  if requestID=0 then
    fRequestID := InterlockedIncrement(GlobalRequestID) else
    fRequestID := requestID;
  fResponseTo := responseTo;
  BSONDocumentBegin;
  fRequestOpCode := opCode;
  Write4(fRequestID);
  Write4(fResponseTo);
  Write4(WIRE_OPCODES[opCode]);
end;

procedure TMongoRequest.BSONWriteParam(const paramDoc: variant);
begin
  if TVarData(paramDoc).VType=varByRef or varVariant then
    BSONWriteParam(PVariant(TVarData(paramDoc).VPointer)^) else
  if VarIsStr(paramDoc) then
    BSONWriteProjection(VariantToUTF8(paramDoc)) else
  if (TVarData(paramDoc).VType=BSONVariantType.VarType) and
     (TBSONVariantData(paramDoc).VKind in [betDoc,betArray]) and
     (TBSONVariantData(paramDoc).VBlob<>nil) then
    WriteBinary(RawByteString(TBSONVariantData(paramDoc).VBlob)) else
    BSONWriteDoc(TDocVariantData(paramDoc)); // for TDocVariant or null
end;

procedure TMongoRequest.ToBSONDocument(var result: TBSONDocument);
begin
  if (fRequestID=0) or (fRequestOpCode=opReply) then
    raise EMongoException.CreateUTF8('No previous proper %.Create() call',[self]);
  if fBSONDocument='' then begin
    BSONDocumentEnd(1,false);
    inherited ToBSONDocument(fBSONDocument);
  end;
  result := fBSONDocument;
end;

procedure TMongoRequest.ToJSON(W: TTextWriter; Mode: TMongoJSONMode);
begin
  if self=nil then begin
    W.AddShort('null');
    exit;
  end;
  W.Add('{');
  W.AddShort('collection:"');
  W.AddJSONEscape(pointer(fFullCollectionName));
  W.AddShort('",opCode:');
  W.AddTypedJSON(TypeInfo(TMongoOperation),fRequestOpCode);
  W.AddShort(',requestID:');
  W.AddU(fRequestID);
  if fResponseTo<>0 then begin
    W.AddShort(',responseTo:');
    W.AddU(fResponseTo);
  end;
  W.Add('}');
end;

function TMongoRequest.ToJSON(Mode: TMongoJSONMode): RawUTF8;
var W: TTextWriter;
    tmp: TTextWriterStackBuffer;
begin
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    ToJSON(W,Mode);
    W.SetText(result);
  finally
    W.Free;
  end;
end;


{ TMongoRequestUpdate }

constructor TMongoRequestUpdate.Create(const FullCollectionName: RawUTF8;
  const Selector, Update: variant; Flags: TMongoUpdateFlags);
begin
  inherited Create(FullCollectionName,opUpdate,0,0);
  fSelector := TVarData(Selector);
  fUpdate := TVarData(Update);
  WriteCollectionName(0,FullCollectionName);
  Write4(byte(Flags));
  BSONWriteParam(Selector);
  BSONWriteParam(Update);
end;

procedure TMongoRequestUpdate.ToJSON(W: TTextWriter; Mode: TMongoJSONMode);
begin
  inherited;
  W.CancelLastChar('}');
  W.AddShort(',selector:');
  AddMongoJSON(variant(fSelector),W,modMongoShell);
  W.AddShort(',update:');
  AddMongoJSON(variant(fUpdate),W,modMongoShell);
  W.Add('}');
end;

{ TMongoRequestInsert }

constructor TMongoRequestInsert.Create(const FullCollectionName: RawUTF8;
  const Documents: array of variant; Flags: TMongoInsertFlags=[]);
var i: integer;
begin
  inherited Create(FullCollectionName,opInsert,0,0);
  WriteCollectionName(byte(Flags),FullCollectionName);
  for i := 0 to high(Documents) do
    BSONWriteParam(Documents[i]);
end;

constructor TMongoRequestInsert.Create(const FullCollectionName: RawUTF8;
  const Documents: TBSONDocument; Flags: TMongoInsertFlags=[]);
begin
  inherited Create(FullCollectionName,opInsert,0,0);
  WriteCollectionName(byte(Flags),FullCollectionName);
  Write(pointer(Documents),Length(Documents));
end;

constructor TMongoRequestInsert.Create(const FullCollectionName: RawUTF8;
  const JSONDocuments: array of PUTF8Char; Flags: TMongoInsertFlags=[]);
var i: integer;
    kind: TBSONElementType;
begin
  inherited Create(FullCollectionName,opInsert,0,0);
  WriteCollectionName(byte(Flags),FullCollectionName);
  for i := 0 to high(JSONDocuments) do
    BSONWriteDocFromJSON(JSONDocuments[i],nil,kind);
end;

{ TMongoRequestDelete }

constructor TMongoRequestDelete.Create(const FullCollectionName: RawUTF8;
  const Selector: variant; Flags: TMongoDeleteFlags=[]);
begin
  inherited Create(FullCollectionName,opDelete,0,0);
  fQuery := TVarData(Selector);
  WriteCollectionName(byte(Flags),FullCollectionName);
  Write4(byte(Flags));
  BSONWriteParam(Selector);
end;

procedure TMongoRequestDelete.ToJSON(W: TTextWriter; Mode: TMongoJSONMode);
begin
  inherited;
  W.CancelLastChar('}');
  W.AddShort(',query:');
  AddMongoJSON(variant(fQuery),W,modMongoShell);
  W.Add('}');
end;

{ TMongoRequestQuery }

constructor TMongoRequestQuery.Create(const FullCollectionName: RawUTF8;
  const Query, ReturnFieldsSelector: variant; NumberToReturn: integer;
  NumberToSkip: integer; Flags: TMongoQueryFlags);
begin
  inherited Create(FullCollectionName,opQuery,0,0);
  fNumberToReturn := NumberToReturn;
  fNumberToSkip := NumberToSkip;
  fQuery := TVarData(Query);
  fReturnFieldsSelector := TVarData(ReturnFieldsSelector);
  WriteCollectionName(byte(Flags),FullCollectionName);
  Write4(NumberToSkip);
  Write4(NumberToReturn);
  BSONWriteParam(Query);
  if TVarData(ReturnFieldsSelector).VType>varNull then
    BSONWriteParam(ReturnFieldsSelector);
end;

procedure TMongoRequestQuery.ToJSON(W: TTextWriter; Mode: TMongoJSONMode);
begin
  inherited;
  W.CancelLastChar('}');
  W.AddShort(',query:');
  AddMongoJSON(variant(fQuery),W,modMongoShell);
  if fReturnFieldsSelector.VType<>varNull then begin
    W.AddShort(',projection:');
    AddMongoJSON(variant(fReturnFieldsSelector),W,modMongoShell);
  end;
  if fNumberToReturn<maxInt then begin
    W.AddShort(',numberToReturn:');
    W.Add(fNumberToReturn);
  end;
  if fNumberToSkip>0 then begin
    W.AddShort(',numberToSkip:');
    W.AddU(fNumberToSkip);
  end;
  W.Add('}');
end;

{ TMongoRequestGetMore }

constructor TMongoRequestGetMore.Create(const FullCollectionName: RawUTF8;
  NumberToReturn: integer; CursorID: Int64);
begin
  inherited Create(FullCollectionName,opGetMore,0,0);
  WriteCollectionName(0,FullCollectionName);
  Write4(NumberToReturn);
  Write8(CursorID);
end;

{ TMongoRequestKillCursor }

constructor TMongoRequestKillCursor.Create(const FullCollectionName: RawUTF8;
  const CursorIDs: array of Int64);
var n: integer;
begin
  if high(CursorIDs)<0 then
    raise EMongoException.CreateUTF8('Invalid %.Create([]) call',[self]);
  inherited Create(FullCollectionName,opKillCursors,0,0);
  Write4(0); // reserved for future use
  n := length(CursorIDs);
  Write4(n);
  SetLength(fCursors,n);
  n := n*sizeof(Int64);
  MoveFast(CursorIDs[0],fCursors[0],n);
  Write(pointer(fCursors),n);
end;

procedure TMongoRequestKillCursor.ToJSON(W: TTextWriter; Mode: TMongoJSONMode);
var i: integer;
begin
  inherited;
  W.CancelLastChar('}');
  W.AddShort(',cursorID:[');
  for i := 0 to high(fCursors) do begin
    W.Add(fCursors[i]);
    W.Add(',');
  end;
  W.CancelLastComma;
  W.Add(']','}');
end;


{ TMongoReplyCursor }

procedure TMongoReplyCursor.Init(const ReplyMessage: TMongoReply);
var Len: integer;
begin
  Len := length(ReplyMessage);
  with PMongoReplyHeader(ReplyMessage)^ do begin
    if (Len<sizeof(TMongoReplyHeader)) or (Header.MessageLength<>Len) or
       (sizeof(TMongoReplyHeader)+NumberReturned*5>Len) then
      raise EMongoException.CreateUTF8('TMongoReplyCursor.Init(len=%)',[len]);
    if Header.OpCode<>WIRE_OPCODES[opReply] then
      raise EMongoException.CreateUTF8('TMongoReplyCursor.Init(OpCode=%)',[Header.OpCode]);
    fRequestID := RequestID;
    fResponseTo := ResponseTo;
    byte(fResponseFlags) := ResponseFlags;
    fCursorID := CursorID;
    fStartingFrom := StartingFrom;
    fNumberReturned := NumberReturned;
  end;
  fReply := ReplyMessage;
  fFirstDocument := PAnsiChar(pointer(fReply))+sizeof(TMongoReplyHeader);
  Rewind;
  fLatestDocIndex := -1;
end;

procedure TMongoReplyCursor.ComputeDocumentsList;
var i, Len: integer;
    P: PAnsiChar;
begin
  if fDocuments<>nil then
    exit;
  Len := length(fReply);
  SetLength(fDocuments,DocumentCount);
  P := fFirstDocument;
  for i := 0 to DocumentCount-1 do begin
    fDocuments[i] := P;
    inc(P,PInteger(P)^); // fast "parsing" of all supplied documents
    if P-pointer(fReply)>Len then
      raise EMongoException.CreateUTF8('ComputeDocumentsList(Document[%])',[i]);
  end;
  if P-pointer(fReply)<>Len then
    raise EMongoException.Create('ComputeDocumentsList(Documents)');
end;

function TMongoReplyCursor.GetOneDocument(index: integer): variant;
begin
  if fLatestDocIndex<>index then begin // naive but efficient cache
    GetDocument(index,fLatestDocValue);
    fLatestDocIndex := index;
  end;
  result := fLatestDocValue;
end;

procedure TMongoReplyCursor.GetDocument(index: integer; var result: variant);
begin
  if cardinal(index)>=cardinal(length(fDocuments)) then
    raise EMongoException.CreateUTF8('TMongoReplyCursor.GetDocument(index %>=%)',
      [index,length(fDocuments)]);
  if fDocuments=nil then
    ComputeDocumentsList;
  BSONToDoc(fDocuments[index],result,0,asDocVariantPerReference);
end;

function TMongoReplyCursor.Next(out doc: variant;
  option: TBSONDocArrayConversion=asDocVariantPerReference): boolean;
var b: PByte;
begin
  if Next(b) then begin
    BSONToDoc(b,doc,0,option);
    result := true;
  end else
    result := false;
end;

function TMongoReplyCursor.Next(out doc: PByte): boolean;
begin
  if fCurrentPosition<DocumentCount then begin
    doc := PByte(fCurrentDocument);
    inc(fCurrentDocument,PInteger(fCurrentDocument)^);
    inc(fCurrentPosition);
    result := true;
  end else begin
    doc := nil;
    result := false;
  end;
end;

function TMongoReplyCursor.Next(out BSON: TBSONDocument): boolean;
var b: PByte;
begin
  if Next(b) then begin
    SetString(BSON,PAnsiChar(b)+4,PInteger(b)^);
    result := true;
  end else
    result := false;
end;

function TMongoReplyCursor.Next(out JSON: RawUTF8; Mode: TMongoJSONMode=modMongoStrict): boolean;
var b: PByte;
begin
  if Next(b) then begin
    JSON := BSONToJSON(b,betDoc,0,Mode);
    result := true;
  end else
    result := false;
end;

procedure TMongoReplyCursor.Rewind;
begin
  fCurrentPosition := 0;
  fCurrentDocument := fFirstDocument;
end;

function TMongoReplyCursor.AppendAllToDocVariantDynArray(var Dest: TVariantDynArray): integer;
var b: PByte;
begin
  result := length(Dest);
  if (fReply='') or (DocumentCount<=0) then
    exit; // nothing to append
  SetLength(Dest,result+DocumentCount);
  Rewind;
  while Next(b) do begin
    BSONToDoc(b,Dest[result],0,asDocVariantPerReference);
    inc(result);
  end;
  if result<>length(Dest) then
    raise EMongoException.Create('Invalid opReply Documents');
end;

procedure TMongoReplyCursor.AppendAllToBSON(Dest: TBSONWriter);
var name: RawUTF8;
    i: integer;
    P: PAnsiChar;
begin
  P := FirstDocument;
  for i := 1 to DocumentCount do begin
    UInt32ToUtf8(Dest.Tag,name); // Dest.Tag = item number in array
    Dest.Tag := Dest.Tag+1;
    Dest.BSONWrite(name,betDoc);
    Dest.Write(P,PInteger(P)^);
    inc(P,PInteger(P)^);
  end;
end;

function TMongoReplyCursor.AppendAllToDocVariant(var Dest: TDocVariantData): integer;
var item: variant;
begin
  if Dest.VarType<>DocVariantType.VarType then
    TDocVariant.New(Variant(Dest),JSON_OPTIONS_FAST);
  result := Dest.Count;
  if (fReply='') or (DocumentCount<=0) then
    exit; // nothing to append
  inc(result,DocumentCount);
  Dest.Capacity := result;
  Rewind;
  while Next(item) do
    Dest.AddItem(item);
  if Dest.Count<>result then
    raise EMongoException.Create('Invalid opReply Documents');
end;

procedure TMongoReplyCursor.FetchAllToJSON(W: TTextWriter; Mode: TMongoJSONMode;
  WithHeader: boolean; MaxSize: Cardinal);
var b: PByte;
begin
  if (fReply='') or (DocumentCount<=0) then begin
    W.AddShort('null');
    exit;
  end;
  if WithHeader and (Mode=modMongoShell) then
    W.Add('{ReplyHeader:{ResponseFlags:%,RequestID:%,ResponseTo:%,CursorID:%,'+
      'StartingFrom:%,NumberReturned:%,ReplyDocuments:[',
      [byte(ResponseFlags),RequestID,ResponseTo,CursorID,StartingFrom,DocumentCount]);
  Rewind;
  while Next(b) do begin
    inc(b,sizeof(integer)); // points to the "e_list" of "int32 e_list #0"
    BSONListToJSON(b,betDoc,W,Mode);
    W.Add(',');
    if (MaxSize>0) and (W.TextLength>MaxSize) then begin
      W.AddShort('...');
      break;
    end;
  end;
  W.CancelLastComma;
  if WithHeader then
    W.Add(']','}');
end;

function TMongoReplyCursor.ToJSON(Mode: TMongoJSONMode; WithHeader: boolean;
  MaxSize: Cardinal): RawUTF8;
var W: TTextWriter;
    tmp: TTextWriterStackBuffer;
begin
  if (fReply='') or (DocumentCount<=0) then
    result := 'null' else begin
    W := TTextWriter.CreateOwnedStream(tmp);
    try
      FetchAllToJSON(W,Mode,WithHeader,MaxSize);
      W.SetText(result);
    finally
      W.Free;
    end;
  end;
end;


{ TMongoConnection }

const
  /// message big enough to retrieve the maximum MongoDB document size
  MONGODB_MAXMESSAGESIZE = BSON_MAXDOCUMENTSIZE+sizeof(TMongoReplyHeader);

constructor TMongoConnection.Create(const aClient: TMongoClient;
  const aServerAddress: RawByteString; aServerPort: integer);
begin
  if aClient=nil then
    raise EMongoException.CreateUTF8('%.Create(nil)',[self]);
  fClient := aClient;
  fServerAddress := trim(aServerAddress);
  if fServerAddress='' then
    fServerAddress := '127.0.0.1';
  fServerPort := aServerPort;
  InitializeCriticalSection(fLock);
end;

destructor TMongoConnection.Destroy;
begin
  try
    try
      Close;
    except
      ; // continue on socket error
    end;
  finally
    DeleteCriticalSection(fLock);
    inherited Destroy;
  end;
end;

procedure TMongoConnection.Open;
begin
  if self=nil then
    raise EMongoException.Create('TMongoConnection(nil).Open');
  if fSocket<>nil then
    raise EMongoConnectionException.Create('Duplicate Open',self);
  try
    fSocket := TCrtSocket.Open(fServerAddress,UInt32ToUtf8(fServerPort),
      cslTCP,Client.ConnectionTimeOut,Client.ConnectionTLS);
  except
    on E: Exception do
      raise EMongoException.CreateUTF8(
        '%.Open unable to connect to MongoDB server %: % [%]',
          [self,Client.ConnectionString,E,E.Message]);
  end;
  fSocket.TCPNoDelay := ord(true); // we buffer all output data before sending
  fSocket.KeepAlive := ord(true);  // do not close the connection without notice
end;

function TMongoConnection.GetOpened: boolean;
begin
  result := (self<>nil) and (fSocket<>nil);
end;

procedure TMongoConnection.Close;
begin
  FreeAndNil(fSocket);
end;

procedure TMongoConnection.GetDocumentsAndFree(Query: TMongoRequestQuery;
  var result: TVariantDynArray);
begin
  result := nil;
  GetRepliesAndFree(Query,ReplyDocVariant,result);
end;

procedure TMongoConnection.GetDocumentsAndFree(Query: TMongoRequestQuery;
  var result: variant);
var ForceOneInstance: boolean;
    docs: TVariantDynArray;
begin
  ForceOneInstance := Query.NumberToReturn=1;
  SetVariantNull(result);
  GetRepliesAndFree(Query,ReplyDocVariant,docs);
  if docs<>nil then
    if ForceOneInstance then
      result := docs[0] else
      TDocVariantData(result).InitArrayFromVariants(docs,JSON_OPTIONS_FAST);
end;

function TMongoConnection.GetDocumentsAndFree(Query: TMongoRequestQuery): variant;
begin
  GetDocumentsAndFree(Query,result);
end;

function TMongoConnection.GetBSONAndFree(Query: TMongoRequestQuery): TBSONDocument;
var W: TBSONWriter;
begin
  W := TBSONWriter.Create(TRawByteStringStream);
  try
    W.BSONDocumentBegin;
    GetRepliesAndFree(Query,ReplyBSON,W); // W.Tag = item number in array
    W.BSONDocumentEnd;
    W.ToBSONDocument(result);
  finally
    W.Free;
  end;
end;

function TMongoConnection.GetJSONAndFree(Query: TMongoRequestQuery; Mode: TMongoJSONMode): RawUTF8;
var W: TTextWriter;
    ReturnAsJSONArray: boolean;
    tmp: TTextWriterStackBuffer;
begin
  ReturnAsJSONArray := Query.NumberToReturn>1;
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    if ReturnAsJSONArray then
      W.Add('[');
    case Mode of
      modNoMongo:     GetRepliesAndFree(Query,ReplyJSONNoMongo,W);
      modMongoStrict: GetRepliesAndFree(Query,ReplyJSONStrict,W);
      modMongoShell:  GetRepliesAndFree(Query,ReplyJSONExtended,W);
    end;
    W.CancelLastComma;
    if ReturnAsJSONArray then
      W.Add(']');
    W.SetText(result);
    if (result='') or (result='[]') or (result='{}') then
      result := 'null';
  finally
    W.Free;
  end;
end;

procedure TMongoConnection.GetRepliesAndFree(Query: TMongoRequestQuery;
  OnEachReply: TOnMongoConnectionReply; var Opaque);
var main, more: TMongoReplyCursor;
    getMore: TMongoRequestGetMore;
    count: integer;
    cursorID: Int64;
begin
  try
    if not Assigned(Query) then
      raise EMongoRequestException.Create('Query=nil',self);
    if not Assigned(OnEachReply) then
      raise EMongoRequestException.Create('OnEachReply=nil',self,Query);
    count := Query.NumberToReturn; // 0 means default return size
    GetCursor(Query,main);
    if main.DocumentCount>0 then begin
      OnEachReply(Query,main,Opaque);
      if count>0 then
        dec(count,main.DocumentCount);
    end;
    cursorID := main.CursorID;
    if cursorID<>0 then
      if (Query.NumberToReturn=0) or ((Query.NumberToReturn>0)and(count>0)) then
      repeat
        getMore := TMongoRequestGetMore.Create(
          Query.FullCollectionName,count,cursorID);
        try
          GetCursor(getMore,more);
          if mrfCursorNotFound in more.ResponseFlags then
            raise EMongoRequestException.Create('GetMore cursor not found',self,Query,more);
          if more.DocumentCount>0 then begin
            OnEachReply(Query,more,Opaque);
            dec(count,more.DocumentCount);
          end;
          cursorID := more.CursorID;
        finally
          getMore.Free;
        end;
      until ((Query.NumberToReturn>0)and(count<=0)) or (cursorID=0);
    if cursorID<>0 then // if cursor not exhausted: need to kill it
      SendAndFree(TMongoRequestKillCursor.Create(Query.FullCollectionName,[cursorID]),true);
  finally
    Query.Free;
  end;
end;

procedure TMongoConnection.ReplyDocVariant(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
begin
  Reply.AppendAllToDocVariantDynArray(TVariantDynArray(Opaque));
end;

procedure TMongoConnection.ReplyJSONStrict(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
var W: TTextWriter absolute Opaque;
begin
  Reply.FetchAllToJSON(W,modMongoStrict,false);
  W.Add(',');
end;

procedure TMongoConnection.ReplyJSONExtended(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
var W: TTextWriter absolute Opaque;
begin
  Reply.FetchAllToJSON(W,modMongoShell,false);
  W.Add(',');
end;

procedure TMongoConnection.ReplyJSONNoMongo(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
var W: TTextWriter absolute Opaque;
begin
  Reply.FetchAllToJSON(W,modNoMongo,false);
  W.Add(',');
end;

procedure TMongoConnection.ReplyBSON(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
begin
  Reply.AppendAllToBSON(TBSONWriter(Opaque));
end;

function TMongoConnection.Send(Request: TMongoRequest): boolean;
var doc: TBSONDocument;
begin
  if not Opened and not Client.ReOpen then
    raise EMongoRequestException.Create('Send: Missing Open',self,Request);
  if Request=nil then
    raise EMongoRequestException.Create('Send(nil)',self);
  Request.ToBSONDocument(doc);
  if (Client.LogRequestEvent<>sllNone) and (Client.Log<>nil) and
     (Client.LogRequestEvent in Client.Log.Family.Level) then
    Client.Log.Log(Client.fLogRequestEvent,Request.ToJSON(modMongoShell),Request);
  result := fSocket.TrySndLow(pointer(doc),length(doc));
end;

function TMongoConnection.SendAndFree(Request: TMongoRequest; NoAcknowledge: boolean): variant;
var cmd: variant;
begin
  SetVariantNull(result);
  try
    if self=nil then
      raise EMongoRequestException.Create('Connection=nil',self,Request);
    Lock;
    try
      if Send(Request) then begin
        if NoAcknowledge or
           (Client.WriteConcern in [wcErrorsIgnored,wcUnacknowledged]) then
          exit;
        case Client.WriteConcern of
          wcAcknowledged:        cmd := 'getLastError';
          wcJournaled:           cmd := BSONVariant(['getLastError',1,'j',true]);
          wcReplicaAcknowledged: cmd := BSONVariant(['getLastError',1,'w',2]);
          else raise EMongoRequestException.CreateUTF8(
            '%.SendAndFree WriteConcern=%',[self,ord(Client.WriteConcern)],self,Request);
        end;
        RunCommand(Request.DatabaseName,cmd,result);
        if not VarIsNull(result.err) then
          raise EMongoRequestException.Create(
            'SendAndFree',self,Request,TDocVariantData(result));
      end else // socket error on sending
        if Client.WriteConcern=wcErrorsIgnored then
          exit else
          raise EMongoRequestOSException.Create('SendAndFree',self,Request);
    finally
      UnLock;
    end;
  finally
    Request.Free;
  end;
end;

procedure TMongoConnection.GetCursor(Request: TMongoRequest; var Result: TMongoReplyCursor);
var reply: TMongoReply;
begin
  GetReply(Request,reply);
  Result.Init(reply);
  if (Client.LogReplyEvent<>sllNone) and (Client.Log<>nil) and
     (Client.LogReplyEvent in Client.Log.Family.Level) then
    Client.Log.Log(Client.LogReplyEvent,
      Result.ToJSON(modMongoShell,True,Client.LogReplyEventMaxSize),Request);
  if mrfQueryFailure in Result.ResponseFlags then
    raise EMongoRequestException.Create('Query failure',self,Request,Result);
end;

const
  RECV_ERROR = '%.GetReply(%): Server response timeout or connection broken, '+
    'probably due to a bad formatted BSON request -> close socket';

procedure TMongoConnection.GetReply(Request: TMongoRequest; out result: TMongoReply);
var Header: TMongoWireHeader;
    HeaderLen, DataLen: integer;
begin
  if self=nil then
    raise EMongoRequestException.Create('Connection=nil',self,Request);
  FillCharFast(Header,sizeof(Header),0);
  try
    Lock;
    if Send(Request) then begin
      HeaderLen := SizeOf(Header);
      if not fSocket.TrySockRecv(@Header,HeaderLen) then
        try
          Close;
        finally
          raise EMongoRequestException.CreateUTF8(RECV_ERROR,[self,'hdr'],self,Request);
        end;
      if Header.MessageLength>MONGODB_MAXMESSAGESIZE then
         raise EMongoRequestException.CreateUTF8('%.GetReply: MessageLength=%',
           [self,Header.MessageLength],self,Request);
      SetLength(result,Header.MessageLength);
      PMongoWireHeader(result)^ := Header;
      DataLen := Header.MessageLength-sizeof(Header);
      if not fSocket.TrySockRecv(@PByteArray(result)[sizeof(Header)],DataLen) then
        try
          Close;
        finally
          raise EMongoRequestException.CreateUTF8(RECV_ERROR,[self,'msg'],self,Request);
        end;
      if Header.ResponseTo=Request.MongoRequestID then
        exit; // success
      case Header.OpCode of
      ord(opMsgOld):
        if Client.Log<>nil then
          Client.Log.Log(sllWarning,'Msg (deprecated) from MongoDB: %',
            [BSONToJSON(@PByteArray(result)[sizeof(Header)],betDoc,DataLen,modMongoShell)],Request);
      ord(opMsg):
        // TODO: parse https://docs.mongodb.com/manual/reference/mongodb-wire-protocol/#op-msg
        if Client.Log<>nil then
          Client.Log.Log(sllWarning,'Msg from MongoDB: %',[EscapeToShort(
            @PByteArray(result)[sizeof(Header)],DataLen)],Request);
      end;
    end;
    // if we reached here, this is due to a socket error or an unexpeted opcode
    raise EMongoRequestException.CreateUTF8(
      '%.GetReply: OpCode=% and ResponseTo=% (expected:%)',
      [self,Header.OpCode,Header.ResponseTo,Request.MongoRequestID],self,Request);
  finally
    UnLock;
  end;
end;

function TMongoConnection.RunCommand(const aDatabaseName: RawUTF8;
  const command: variant; var returnedValue: variant; flags: TMongoQueryFlags): RawUTF8;
begin
  GetDocumentsAndFree(
    TMongoRequestQuery.Create(aDatabaseName+'.$cmd',command,null,1,0,flags),
    returnedValue);
  with _Safe(returnedValue)^ do
    if GetValueOrDefault('ok',1)<>0 then
      result := '' else
      if not GetAsRawUTF8('errmsg',result) then
        result := 'unspecified error';
end;

function TMongoConnection.RunCommand(const aDatabaseName: RawUTF8;
  const command: variant; var returnedValue: TBSONDocument;
  flags: TMongoQueryFlags): boolean;
var item: TBSONElement;
begin
  returnedValue := GetBSONAndFree(
    TMongoRequestQuery.Create(aDatabaseName+'.$cmd',command,null,1,0,flags));
  result := true;
  item.FromDocument(returnedValue);
  if item.DocItemToInteger('ok',1)=0 then
    result := false;
end;

procedure TMongoConnection.Lock;
begin
  EnterCriticalSection(fLock);
  inc(fLocked);
end;

procedure TMongoConnection.UnLock;
begin
  dec(fLocked);
  LeaveCriticalSection(fLock);
end;

function TMongoConnection.GetLocked: boolean;
begin
  result := (self<>nil) and (fLocked>0);
end;


{ EMongoConnectionException }

constructor EMongoConnectionException.Create(const aMsg: string;
  aConnection: TMongoConnection);
begin
  inherited Create(aMsg);
  fConnection := aConnection;
end;

constructor EMongoConnectionException.CreateUTF8(const Format: RawUTF8; const Args: array of const;
  aConnection: TMongoConnection);
begin
  inherited CreateUTF8(Format,Args);
  fConnection := aConnection;
end;


{ EMongoRequestException }

constructor EMongoRequestException.Create(const aMsg: string;
  aConnection: TMongoConnection; aRequest: TMongoRequest);
begin
  inherited Create(aMsg,aConnection);
  fRequest := aRequest;
end;

constructor EMongoRequestException.CreateUTF8(const Format: RawUTF8;
  const Args: array of const; aConnection: TMongoConnection;
  aRequest: TMongoRequest);
begin
  inherited CreateUTF8(Format,Args,aConnection);
  fRequest := aRequest;
end;

constructor EMongoRequestException.Create(const aMsg: string;
  aConnection: TMongoConnection; aRequest: TMongoRequest;
  const aError: TMongoReplyCursor);
begin
  Create(aMsg,aConnection,aRequest);
  fError := aError;
end;

constructor EMongoRequestException.Create(const aMsg: string;
  aConnection: TMongoConnection; aRequest: TMongoRequest;
  const aErrorDoc: TDocVariantData);
begin
  Create(aMsg,aConnection,aRequest);
  fErrorDoc := variant(aErrorDoc);
end;

{$ifndef NOEXCEPTIONINTERCEPT}
function EMongoRequestException.CustomLog(WR: TTextWriter;
  const Context: TSynLogExceptionContext): boolean;
begin
  inherited CustomLog(WR,Context);
  if fRequest<>nil then begin
    WR.AddInstanceName(fRequest,':');
    fRequest.ToJSON(WR,modMongoShell);
  end;
  if fError.Reply<>'' then
    fError.FetchAllToJSON(WR,modMongoShell,True);
  result := false; // log stack trace
end;
{$endif}

function EMongoRequestException.GetErrorDoc: variant;
begin
  if TVarData(fErrorDoc).VType=varEmpty then begin
    if not fError.Next(fErrorDoc) then
      SetVariantNull(fErrorDoc);
  end;
  result := fErrorDoc;
end;


{ EMongoRequestOSException }

constructor EMongoRequestOSException.Create(const aMsg: string;
  aConnection: TMongoConnection; aRequest: TMongoRequest=nil);
begin
  fSystemLastError := GetLastError;
  CreateUTF8('%: % (%)',[aMsg,SysErrorMessage(fSystemLastError),fSystemLastError],
    aConnection,aRequest);
end;


{ EMongoDatabaseException }

constructor EMongoDatabaseException.Create(const aMsg: string;
  aDatabase: TMongoDatabase);
begin
  inherited Create(aMsg,aDatabase.Client.Connections[0]);
  fDatabase := aDatabase;
end;

constructor EMongoDatabaseException.CreateUTF8(const Format: RawUTF8;
  const Args: array of const; aDatabase: TMongoDatabase);
begin
  inherited CreateUTF8(Format,Args,aDatabase.Client.Connections[0]);
  fDatabase := aDatabase;
end;

{$ifndef NOEXCEPTIONINTERCEPT}
function EMongoDatabaseException.CustomLog(WR: TTextWriter;
  const Context: TSynLogExceptionContext): boolean;
begin
  inherited CustomLog(WR,Context);
  WR.AddJSONEscape(['Database',fDatabase.Name]);
  result := false; // log stack trace
end;
{$endif}


{ TMongoClient }

constructor TMongoClient.Create(const Host: RawUTF8; Port: integer;
  aTLS: boolean; const SecondaryHostCSV, SecondaryPortCSV: RawUTF8);
const PROT: array[boolean] of string[1] = ('', 's');
var secHost: TRawUTF8DynArray;
    secPort: TIntegerDynArray;
    nHost, i: integer;
begin
  fConnectionTimeOut := 30000;
  fConnectionTLS := aTLS;
  fLogReplyEventMaxSize := 1024;
  fGracefulReconnect.Enabled := true;
  FormatUTF8('mongodb%://%:%',[PROT[aTLS],Host,Port],fConnectionString);
  CSVToRawUTF8DynArray(pointer(SecondaryHostCSV),secHost);
  nHost := length(secHost);
  SetLength(fConnections,nHost+1);
  fConnections[0] := TMongoConnection.Create(self,Host,Port);
  if nHost>0 then begin
    CSVToIntegerDynArray(pointer(SecondaryPortCSV),secPort);
    for i := 0 to nHost-1 do begin
      if i>high(secPort) then
        Port := MONGODB_DEFAULTPORT else
        Port := secPort[i];
      fConnections[i+1] := TMongoConnection.Create(self,secHost[i],Port);
      fConnectionString := FormatUTF8('%,%:%',[fConnectionString,secHost[i],Port]);
    end;
  end;
  fDatabases := TRawUTF8List.Create([fObjectsOwned,fNoDuplicate,fCaseSensitive]);
end;

destructor TMongoClient.Destroy;
var i: integer;
begin
  for i := 0 to high(fConnections) do
    FreeAndNil(fConnections[i]);
  FreeAndNil(fDatabases);
  inherited;
end;

procedure TMongoClient.SetLog(LogClass: TSynLogClass;
  RequestEvent, ReplyEvent: TSynLogInfo; ReplyEventMaxSize: cardinal);
begin
  fLog := LogClass.Add;
  LogRequestEvent := RequestEvent;
  LogReplyEvent := ReplyEvent;
  LogReplyEventMaxSize := ReplyEventMaxSize;
end;

function TMongoClient.ServerBuildInfoText: RawUTF8;
begin
  with _Safe(ServerBuildInfo)^ do
    if Count=0 then
      result := '' else begin
      FormatUTF8('MongoDB % %',[U['version'],U['javascriptEngine']],result);
      with A['storageEngines']^ do begin
        // "storageEngines":["devnull","ephemeralForTest","mmapv1","wiredTiger"]
        DeleteByValue('devnull');
        DeleteByValue('ephemeralForTest');
        if Count>0 then
          result := result+' '+ToCSV;
      end;
    end;
end;

function TMongoClient.GetOneReadConnection: TMongoConnection;
  function GetUnlockedSecondaryIndex: PtrInt;
  var retry: integer;
  begin
    if Length(fConnections)=1 then // no secondary? use primary
      result := 0 else begin
      for retry := 1 to 100 do begin // search for an inactive connection
        result := fLatestReadConnectionIndex; // simple round-robin pattern
        if result=high(fConnections) then
          if ReadPreference=rpSecondary then
            result := 1 else
            result := 0 else
          inc(result); // thread-safety is not an issue here
        if (retry<=length(fConnections)) and not fConnections[result].Opened then
        try
          fConnections[result].Open;
        except
          on E: Exception do
          begin
            SleepHiRes(2);
            continue;
          end;
        end;
        if fConnections[result].Opened then
          if fConnections[result].Locked then
            if retry mod length(fConnections)=0 then
              SleepHiRes(2) else
              continue else
            break;
      end;
      if not fConnections[result].Opened then
        result := 0; // safe fallback to primary member in worst case
      fLatestReadConnectionIndex := result;
    end;
  end;
var n, retry: integer;
begin
  case ReadPreference of
  rpPrimaryPreferred:
    if fConnections[0].Locked then
      result := fConnections[GetUnlockedSecondaryIndex] else
      result := fConnections[0];
  rpSecondary, rpSecondaryPreferred:
    result := fConnections[GetUnlockedSecondaryIndex];
  rpNearest: begin
    n := Length(fConnections);
    for retry := 1 to n*2 do begin
      result := fConnections[Random32(n)];
      if not result.Locked then
        exit;
    end;
    result := fConnections[0]; // falback to the main instance
  end else // rpPrimary or not handled yet
    result := fConnections[0];
  end;
end;

function TMongoClient.Open(const DatabaseName: RawUTF8): TMongoDatabase;
begin
  if self=nil then
    result := nil else begin
    result := fDatabases.GetObjectFrom(DatabaseName);
    if result=nil then begin // not already opened -> try now from primary host
      if not fConnections[0].Opened then begin
        fConnections[0].Open;
        AfterOpen(0);
      end;
      result := TMongoDatabase.Create(Self,DatabaseName);
      fDatabases.AddObjectUnique(DatabaseName,@result);
    end;
  end;
end;

function PasswordDigest(const UserName,Password: RawUTF8): RawUTF8;
begin
  result := MD5(UserName+':mongo:'+PassWord);
end;

function TMongoClient.OpenAuth(const DatabaseName,UserName,PassWord: RawUTF8;
  ForceMongoDBCR: boolean): TMongoDatabase;
var digest: RawByteString;
    i: PtrInt;
begin
  if (self=nil) or (DatabaseName='') or (UserName='') or (PassWord='') then
    raise EMongoException.CreateUTF8('Invalid %.OpenAuth("%") call',[self,DatabaseName]);
  result := fDatabases.GetObjectFrom(DatabaseName);
  if result=nil then  // not already opened -> try now
  try
    // ensure we are opened and authenticated on all connections
    for i := 0 to High(fConnections) do
      if not fConnections[i].Opened then
        try
          fConnections[i].Open; // socket connection
          AfterOpen(i); // need ServerBuildInfoNumber just below
          digest := PasswordDigest(UserName,Password);
          Auth(DatabaseName,UserName,digest,ForceMongoDBCR, i);
          with fGracefulReconnect do
            if Enabled and (EncryptedDigest='') then begin
              ForcedDBCR := ForceMongoDBCR;
              User := UserName;
              Database := DatabaseName;
              EncryptedDigest := CryptDataForCurrentUser(digest,Database,true);
            end;
        except
          fConnections[i].Close;
          raise;
        end;
    result := TMongoDatabase.Create(Self,DatabaseName);
    fDatabases.AddObjectUnique(DatabaseName,@result);
  finally
    FillZero(digest);
  end;
end;

procedure TMongoClient.Auth(const DatabaseName,UserName,Digest: RawUTF8;
  ForceMongoDBCR: boolean; ConnectionIndex: PtrInt);
var res,bson: variant;
    err,nonce,first,key,user,msg,rnonce: RawUTF8;
    payload: RawByteString;
    rnd: TAESBlock;
    sha: TSHA1;
    salted,client,stored,server: TSHA1Digest;
    resp: TDocVariantData;

  procedure CheckPayload;
  var bin: PVariant;
  begin
    if err<>'' then
      exit;
    if _Safe(res)^.GetAsPVariant('payload',bin) and
       BSONVariantType.ToBlob(bin^,payload) then
      resp.InitCSV(pointer(payload),JSON_OPTIONS_FAST,'=',',') else
      err := 'missing or invalid returned payload';
  end;

begin // caller should have made fConnections[0].Open
  if (self=nil) or (DatabaseName='') or (UserName='') or (Digest='') then
    raise EMongoException.CreateUTF8('Invalid %.Auth("%") call',[self,DatabaseName]);
  if ForceMongoDBCR or (ServerBuildInfoNumber<3000000) then begin
    // MONGODB-CR
    // http://docs.mongodb.org/meta-driver/latest/legacy/implement-authentication-in-driver
    bson := BSONVariant(['getnonce',1]);
    err := fConnections[ConnectionIndex].RunCommand(DatabaseName,bson,res);
    if (err='') and not _Safe(res)^.GetAsRawUTF8('nonce',nonce) then
      err := 'missing returned nonce';
    if err<>'' then
      raise EMongoException.CreateUTF8('%.OpenAuthCR("%") step1: % - res=%',
        [self,DatabaseName,err,res]);
    key := MD5(nonce+UserName+Digest);
    bson := BSONVariant(['authenticate',1,'user',UserName,'nonce',nonce,'key',key]);
    err := fConnections[ConnectionIndex].RunCommand(DatabaseName,bson,res);
    if err<>'' then
      raise EMongoException.CreateUTF8('%.OpenAuthCR("%") step2: % - res=%',
        [self,DatabaseName,err,res]);
  end else begin
    // SCRAM-SHA-1
    // https://tools.ietf.org/html/rfc5802#section-5
    user := StringReplaceAll(UserName,['=','=3D', ',','=2C']);
    TAESPRNG.Main.FillRandom(rnd);
    nonce := BinToBase64(@rnd,sizeof(rnd));
    FormatUTF8('n=%,r=%',[user,nonce],first);
    BSONVariantType.FromBinary('n,,'+first,bbtGeneric,bson);
    err := fConnections[ConnectionIndex].RunCommand(DatabaseName,BSONVariant([
      'saslStart',1,'mechanism','SCRAM-SHA-1','payload',bson,'autoAuthorize',1]),res);
    CheckPayload;
    if err='' then begin
      rnonce := resp.U['r'];
      if copy(rnonce,1,length(nonce))<>nonce then
        err := 'returned invalid nonce';
    end;
    if err<>'' then
      raise EMongoException.CreateUTF8('%.OpenAuthSCRAM("%") step1: % - res=%',
        [self,DatabaseName,err,res]);
    key := 'c=biws,r='+rnonce;
    PBKDF2_HMAC_SHA1(Digest,Base64ToBin(resp.U['s']),UTF8ToInteger(resp.U['i']),salted);
    HMAC_SHA1(salted,'Client Key',client);
    sha.Full(@client,SizeOf(client),stored);
    msg := first+','+RawUTF8(payload)+','+key;
    HMAC_SHA1(stored,msg,stored);
    XorMemory(@client,@stored,SizeOf(client));
    HMAC_SHA1(salted,'Server Key',server);
    HMAC_SHA1(server,msg,server);
    msg := key+',p='+BinToBase64(@client,SizeOf(client));
    BSONVariantType.FromBinary(msg,bbtGeneric,bson);
    err := fConnections[ConnectionIndex].RunCommand(DatabaseName,BSONVariant([
      'saslContinue',1,'conversationId',res.conversationId,'payload',bson]),res);
    resp.Clear;
    CheckPayload;
    if (err='') and (resp.U['v']<>BinToBase64(@server,SizeOf(server))) then
      err := 'Server returned an invalid signature';
    if err<>'' then
      raise EMongoException.CreateUTF8('%.OpenAuthSCRAM("%") step2: % - res=%',
        [self,DatabaseName,err,res]);
    if not res.done then begin
      // third empty challenge may be required
      err := fConnections[ConnectionIndex].RunCommand(DatabaseName,BSONVariant([
        'saslContinue',1,'conversationId',res.conversationId,'payload','']),res);
     if (err='') and not res.done then
       err := 'SASL conversation failed to complete';
      if err<>'' then
        raise EMongoException.CreateUTF8('%.OpenAuthSCRAM("%") step3: % - res=%',
          [self,DatabaseName,err,res]);
    end;
  end;
end;

procedure TMongoClient.AfterOpen(ConnectionIndex: PtrInt);
begin
  if VarIsEmptyOrNull(fServerBuildInfo) then begin
    fConnections[ConnectionIndex].RunCommand('admin','buildinfo',fServerBuildInfo);
    with _Safe(fServerBuildInfo)^.A['versionArray']^ do
      if Count=4 then
        fServerBuildInfoNumber := // e.g. 2040900 for MongoDB 2.4.9
          integer(Values[0])*1000000+integer(Values[1])*10000+
          integer(Values[2])*100+integer(Values[3]);
  end;
end;

function TMongoClient.ReOpen: boolean;
var digest: RawByteString;
begin
  result := false;
  with fGracefulReconnect do
    if Enabled then
    try
      if fLog<>nil then
        fLog.Enter(self,'ReOpen: graceful reconnect');
      fConnections[0].Open;
      if EncryptedDigest<>'' then
        try
          digest := CryptDataForCurrentUser(EncryptedDigest,Database,false);
          Auth(Database,User,digest,ForcedDBCR,0);
        finally
          FillZero(digest);
        end;
      result := true;
    except
      fConnections[0].Close;
      raise;
    end;
end;

function TMongoClient.GetBytesReceived: Int64;
var i: integer;
begin
  result := 0;
  if self<>nil then
    for i := 0 to high(fConnections) do
      with fConnections[i] do
      if fSocket<>nil then
        inc(result,fSocket.BytesIn);
end;

function TMongoClient.GetBytesSent: Int64;
var i: integer;
begin
  result := 0;
  if self<>nil then
    for i := 0 to high(fConnections) do
      with fConnections[i] do
      if fSocket<>nil then
        inc(result,fSocket.BytesOut);
end;

function TMongoClient.GetBytesTransmitted: Int64;
var i: integer;
begin
  result := 0;
  if self<>nil then
    for i := 0 to high(fConnections) do
      with fConnections[i] do
      if fSocket<>nil then
        inc(result,fSocket.BytesIn+fSocket.BytesOut);
end;


{ TMongoDatabase }

constructor TMongoDatabase.Create(aClient: TMongoClient;
  const aDatabaseName: RawUTF8);
var colls: TBSONIterator;
    full,db,coll: RawUTF8;
    resp,batch: variant;
    mc: TMongoCollection;
    ndx: integer;
begin
  fClient := aClient;
  fName := aDatabaseName;
  fCollections := TRawUTF8List.Create([fObjectsOwned,fNoDuplicate,fCaseSensitive]);
  if fClient.ServerBuildInfoNumber<3000000 then begin
    if colls.Init(Client.Connections[0].GetBSONAndFree(TMongoRequestQuery.Create(
      aDatabaseName+'.system.namespaces',null,'name',maxInt))) then
    // e.g. [ {name:"test.system.indexes"}, {name:"test.test"} ]
    while colls.Next do begin
      full := colls.item.DocItemToRawUTF8('name');
      if full<>'' then begin
        split(full,'.',db,coll);
        if db<>aDatabaseName then
          raise EMongoConnectionException.CreateUTF8(
            '%.Create: invalid [%] collection name for DB [%]',
            [self,full,aDatabaseName],Client.Connections[0]);
        mc := TMongoCollection.Create(self,coll);
        fCollections.AddObjectUnique(coll,@mc);
      end;
    end;
  end else begin
    RunCommand('listCollections',resp);
    if _Safe(resp)^.GetValueByPath('cursor.firstBatch',batch) then
      with _Safe(batch)^ do
      for ndx := 0 to Count-1 do
        if _Safe(Values[ndx]).GetAsRawUTF8('name',coll) then begin
          mc := TMongoCollection.Create(self,coll);
          fCollections.AddObjectUnique(coll,@mc);
        end;
  end;
end;

destructor TMongoDatabase.Destroy;
begin
  FreeAndNil(fCollections);
  inherited;
end;

function TMongoDatabase.CreateUser(const UserName,Password: RawUTF8;
  const roles: variant): RawUTF8;
var res: variant;
    usr: TDocVariantData;
begin
  usr.InitObject(['createUser',UserName,'pwd',PasswordDigest(UserName,Password),
     'digestPassword',false,'roles',roles],JSON_OPTIONS_FAST);
  if Client.ServerBuildInfoNumber>=4000000 then
    usr.AddValue('mechanisms',_ArrFast(['SCRAM-SHA-1']));
    // note: passwordDigestor:"client" fails
  result := RunCommand(variant(usr),res);
end;

function TMongoDatabase.CreateUserForThisDatabase(const UserName,Password: RawUTF8;
  allowWrite: Boolean): RawUTF8;
const RW: array[boolean] of RawUTF8 = ('read','readWrite');
begin
  result := CreateUser(UserName,Password,
    BSONVariant('[{role:?,db:?}]',[],[RW[allowWrite],Name]));
end;

function TMongoDatabase.DropUser(const UserName: RawUTF8): RawUTF8;
var res: variant;
begin
  result := RunCommand(BSONVariant(['dropUser',UserName]),res);
end;

function TMongoDatabase.GetCollection(const Name: RawUTF8): TMongoCollection;
begin
  result := GetCollectionOrNil(Name);
  if result=nil then
    raise EMongoDatabaseException.CreateUTF8('%.GetCollection("%")',[self,Name],self);
end;

function TMongoDatabase.GetCollectionOrCreate(const Name: RawUTF8): TMongoCollection;
begin
  result := GetCollectionOrNil(Name);
  if result=nil then
    if self<>nil then begin
      result := TMongoCollection.Create(self,Name);
      fCollections.AddObjectUnique(Name,@result);
    end;
end;

function TMongoDatabase.GetCollectionOrNil(const Name: RawUTF8): TMongoCollection;
begin
  if self=nil then
    result := nil else
    result := fCollections.GetObjectFrom(Name);
end;

function TMongoDatabase.RunCommand(const command: variant;
  var returnedValue: variant): RawUTF8;
begin
  result := Client.Connections[0].RunCommand(Name,Command,returnedValue);
end;

function TMongoDatabase.RunCommand(const command: variant;
  var returnedValue: TBSONDocument): boolean;
begin
  result := Client.Connections[0].RunCommand(Name,Command,returnedValue);
end;


{ TMongoCollection }

constructor TMongoCollection.Create(aDatabase: TMongoDatabase;
  const aCollectionName: RawUTF8);
begin
  fDatabase := aDatabase;
  fName := aCollectionName;
  fFullCollectionName := fDatabase.Name+'.'+fName;
end;

function TMongoCollection.AggregateCallFromJson(const pipelineJSON: RawUTF8;
  var reply,res: variant): boolean;
begin // see http://docs.mongodb.org/manual/reference/command/aggregate
  if fDatabase.Client.ServerBuildInfoNumber<2020000 then
    raise EMongoException.Create('Aggregation needs MongoDB 2.2 or later');
  if fDatabase.Client.ServerBuildInfoNumber>=3060000 then begin
    // db.runCommand({aggregate:"test",pipeline:[{$group:{_id:null,max:{$max:"$_id"}}}],cursor:{}})
    Database.RunCommand(BSONVariant('{aggregate:"%",pipeline:[%],cursor:{}}',[Name,pipelineJSON],[]),reply);
    // {"cursor":{"firstBatch":[{"_id":null,"max":1510}],"id":0,"ns":"db.test"},"ok":1}
    res := reply.cursor;
    if not VarIsNull(res) then
      res := res.firstBatch;
  end else begin
    // db.runCommand({aggregate:"test",pipeline:[{$group:{_id:null,max:{$max:"$_id"}}}]})
    Database.RunCommand(BSONVariant('{aggregate:"%",pipeline:[%]}',[Name,pipelineJSON],[]),reply);
    // { "result" : [ { "_id" : null, "max" : 1250 } ], "ok" : 1 }
    res := reply.result;
  end;
  result := not VarIsNull(res);
end;

function TMongoCollection.AggregateDoc(Operators: PUTF8Char;
  const Params: array of const): variant;
begin
  result := AggregateDocFromJson(FormatUTF8(Operators,Params));
end;

function TMongoCollection.AggregateJSON(Operators: PUTF8Char;
  const Params: array of const; Mode: TMongoJSONMode): RawUTF8;
begin
  result := AggregateJSONFromJson(FormatUTF8(Operators,Params),Mode);
end;

function TMongoCollection.AggregateCallFromVariant(const pipelineArray: variant; var reply,res: variant): boolean;
begin // see http://docs.mongodb.org/manual/reference/command/aggregate
  if fDatabase.Client.ServerBuildInfoNumber<2020000 then
    raise EMongoException.Create('Aggregation needs MongoDB 2.2 or later');
  if fDatabase.Client.ServerBuildInfoNumber>=3060000 then begin
    // db.runCommand({aggregate:"test",pipeline:[{$group:{_id:null,max:{$max:"$_id"}}}],cursor:{}})
    Database.RunCommand(BSONVariant(['aggregate',name,'pipeline',pipelineArray,'cursor','{','}']),reply);
    // {"cursor":{"firstBatch":[{"_id":null,"max":1510}],"id":0,"ns":"db.test"},"ok":1}
    res := reply.cursor;
    if not VarIsNull(res) then
      res := res.firstBatch;
  end else begin
    // db.runCommand({aggregate:"test",pipeline:[{$group:{_id:null,max:{$max:"$_id"}}}]})
    Database.RunCommand(BSONVariant(['aggregate',name,'pipeline',pipelineArray]),reply);
    // { "result" : [ { "_id" : null, "max" : 1250 } ], "ok" : 1 }
    res := reply.result;
  end;
  result := not VarIsNull(res);
end;

function TMongoCollection.AggregateDocFromVariant(const pipelineArray: variant): variant;
var reply: variant;
begin
  if AggregateCallFromVariant(pipelineArray,reply,result) then
    TDocVariant.GetSingleOrDefault(result,result,result) else
    SetVariantNull(result);
end;

function TMongoCollection.AggregateJSONFromVariant(const pipelineArray: variant;
  Mode: TMongoJSONMode=modMongoStrict): RawUTF8;
var reply,res: variant;
begin
  if AggregateCallFromVariant(pipelineArray,reply,res) then
    result := VariantSaveMongoJSON(res,Mode) else
    result := '';
end;

function TMongoCollection.AggregateDocFromJson(const PipelineJSON: RawUTF8): variant;
var reply: variant;
begin
  if AggregateCallFromJSON(PipelineJSON,reply,result) then
    TDocVariant.GetSingleOrDefault(result,result,result) else
    SetVariantNull(result);
end;

function TMongoCollection.AggregateJSONFromJson(const PipelineJSON: RawUTF8;
  Mode: TMongoJSONMode=modMongoStrict): RawUTF8;
var reply,res: variant;
begin
  if AggregateCallFromJson(PipelineJSON,reply,res) then
    result := VariantSaveMongoJSON(res,Mode) else
    result := '';
end;

function TMongoCollection.Drop: RawUTF8;
var res: Variant;
begin
  if self=nil then begin
    result := 'No collection';
    exit;
  end;
  if Database.Client.Log<>nil then
    Database.Client.Log.Enter('Drop %',[Name],self);
  result := fDatabase.RunCommand(BSONVariant('{drop:?}',[],[Name]),res);
  Database.Client.Log.Log(sllTrace,'Drop("%")->%',[Name,res],self);
  if result='' then
    Database.fCollections.Delete(Name);
end;

procedure TMongoCollection.EnsureIndex(const Keys, Options: variant);
var doc,res: variant;
    indexName: RawUTF8;
    ndx,order: integer;
    useCommand: Boolean;
begin
  if (self=nil) or (Database=nil) then
    exit;
  if Database.Client.Log<>nil then
    Database.Client.Log.Enter('EnsureIndex %',[Name],self);
  if DocVariantData(Keys)^.Kind<>dvObject then
    raise EMongoException.CreateUTF8('%[%].EnsureIndex(Keys?)',[self,FullCollectionName]);
  useCommand := fDatabase.Client.ServerBuildInfoNumber>=2060000;
  doc := _ObjFast(['key', Keys]);
  if not useCommand then
    doc.ns := FullCollectionName;
  if DocVariantType.IsOfType(Options) then
    with _Safe(Options)^ do
    for ndx := 0 to Count-1 do
      if Names[ndx]='name' then
        indexName := VariantToUTF8(Values[ndx]) else
        TDocVariantData(doc).AddValue(Names[ndx],Values[ndx]);
  if indexName='' then begin
    with _Safe(Keys)^ do
    for ndx := 0 to Count-1 do begin
      indexName := indexName+Names[ndx]+'_';
      order := VariantToIntegerDef(Values[ndx],10);
      if order=-1 then
        indexName := indexName+'_' else
      if order<>1 then
        raise EMongoException.CreateUTF8('%[%].EnsureIndex() on order {%:%}',
          [self,FullCollectionName,Names[ndx],Values[ndx]]);
    end;
  end;
  if length(FullCollectionName)+length(indexName)>120 then
    raise EMongoException.CreateUTF8(
      '%[%].EnsureIndex() computed name > 128 chars: please set as option',
      [self,FullCollectionName]);
  doc.name := indexName;
  if useCommand then
    fDatabase.RunCommand(BSONVariant(
      '{ createIndexes: ?, indexes: [?] }',[],[Name,doc]),res) else
    fDatabase.GetCollectionOrCreate('system.indexes').Insert([doc]);
  Database.Client.Log.Log(sllTrace,'EnsureIndex("%",%)->%',[Name,doc,res],self);
end;

procedure TMongoCollection.EnsureIndex(const Keys: array of RawUTF8;
  Ascending, Unique: boolean);
const Order: array[boolean] of integer = (-1,1);
var k,opt: variant;
    A: integer;
begin
  if high(Keys)<0 then
    exit; // no column name
  TDocVariant.New(k);
  for A := 0 to high(Keys) do
    TDocVariantData(k).AddValue(Keys[A],Order[Ascending]);
  if Unique then
    opt := _ObjFast(['unique',true]);
  EnsureIndex(k,opt);
end;

function TMongoCollection.Count: Int64;
var res: variant;
begin
  fDatabase.RunCommand(BSONVariant(['count',Name]),res);
  result := _Safe(res)^.GetValueOrDefault('n',0);
end;

function TMongoCollection.FindCount(const Query: variant): Int64;
var res: variant;
begin
  fDatabase.RunCommand(BSONVariant(['count',Name,'query',Query]),res);
  result := _Safe(res)^.GetValueOrDefault('n',0);
end;

function TMongoCollection.FindCount(Criteria: PUTF8Char;
  const Args,Params: array of const;
  MaxNumberToReturn, NumberToSkip: integer): Int64;
var cmd: RawUTF8;
    res: variant;
begin
  FormatUTF8('{count:"%",query:%',[Name,FormatUTF8(Criteria,Args,Params,true)],cmd);
  if MaxNumberToReturn>0 then
    cmd := FormatUTF8('%,limit:%',[cmd,MaxNumberToReturn]);
  if NumberToSkip>0 then
    cmd := FormatUTF8('%,skip:%',[cmd,NumberToSkip]);
  fDatabase.RunCommand(BSONVariant(cmd+'}'),res);
  result := _Safe(res)^.GetValueOrDefault('n',0);
end;

function TMongoCollection.IsEmpty: boolean;
var res: variant;
begin // much faster than Count>0 for huge collections
  res := FindDoc(BSONVariant('{$query:{}}'),BSONVariant(['_id',1]));
  result := VarIsEmptyOrNull(res);
end;

function TMongoCollection.FindBSON(const Criteria, Projection: Variant;
  NumberToReturn, NumberToSkip: integer; Flags: TMongoQueryFlags): TBSONDocument;
begin
  result := Database.Client.GetOneReadConnection.GetBSONAndFree(
    TMongoRequestQuery.Create(fFullCollectionName,
      Criteria,Projection,NumberToReturn,NumberToSkip,Flags));
end;

function TMongoCollection.FindDoc(const Criteria, Projection: Variant;
  NumberToReturn, NumberToSkip: integer; Flags: TMongoQueryFlags): variant;
begin
  Database.Client.GetOneReadConnection.GetDocumentsAndFree(
    TMongoRequestQuery.Create(fFullCollectionName,
      Criteria,Projection,NumberToReturn,NumberToSkip,Flags),result);
end;

function TMongoCollection.FindDoc(Criteria: PUTF8Char;
  const Params: array of const; NumberToReturn, NumberToSkip: integer;
  Flags: TMongoQueryFlags): variant;
begin
  result := FindDoc(BSONVariant(Criteria,[],Params),null,
    NumberToReturn,NumberToSkip,Flags);
end;

procedure TMongoCollection.FindDocs(Criteria: PUTF8Char;
  const Params: array of const; var result: TVariantDynArray;
  const Projection: variant; NumberToReturn, NumberToSkip: integer;
  Flags: TMongoQueryFlags);
begin
  Database.Client.GetOneReadConnection.GetDocumentsAndFree(
    TMongoRequestQuery.Create(fFullCollectionName,
      BSONVariant(Criteria,[],Params),Projection,
      NumberToReturn,NumberToSkip,Flags),result);
end;

function TMongoCollection.FindDocs(Criteria: PUTF8Char; const Params: array of const;
  const Projection: variant; NumberToReturn,NumberToSkip: integer;
  Flags: TMongoQueryFlags): TVariantDynArray;
begin
  FindDocs(Criteria,Params,result,Projection,NumberToReturn,NumberToSkip,Flags);
end;

function TMongoCollection.FindOne(const _id: TBSONObjectID): variant;
begin
  result := FindOne(['_id',_id.ToVariant]);
end;

function TMongoCollection.FindOne(const _id: variant): variant;
begin
  result := FindOne(['_id',_id]);
end;

function TMongoCollection.FindOne(const NameValuePairs: array of const;
  ReturnNewObjectIfNotFound: boolean): variant;
begin
  result := FindDoc(BSONVariant(NameValuePairs),null,1);
  if ReturnNewObjectIfNotFound and VarIsEmptyOrNull(result) then
    TDocVariantData(Result).InitObject(NameValuePairs,JSON_OPTIONS_FAST);
end;

procedure TMongoCollection.FindDocs(var result: TVariantDynArray;
  const Projection: variant; NumberToReturn, NumberToSkip: integer;
  Flags: TMongoQueryFlags);
begin
  Database.Client.GetOneReadConnection.GetDocumentsAndFree(
    TMongoRequestQuery.Create(fFullCollectionName,null,Projection,
      NumberToReturn,NumberToSkip,Flags),result);
end;

function TMongoCollection.FindJSON(const Criteria, Projection: Variant;
  NumberToReturn, NumberToSkip: integer; Flags: TMongoQueryFlags;
  Mode: TMongoJSONMode): RawUTF8;
begin
  result := Database.Client.GetOneReadConnection.GetJSONAndFree(
    TMongoRequestQuery.Create(fFullCollectionName,
      Criteria,Projection,NumberToReturn,NumberToSkip,Flags),Mode);
end;

function TMongoCollection.FindJSON(Criteria: PUTF8Char;
  const Params: array of const; NumberToReturn, NumberToSkip: integer;
  Flags: TMongoQueryFlags; Mode: TMongoJSONMode): RawUTF8;
begin
  result := FindJSON(BSONVariant(Criteria,[],Params),null,
    NumberToReturn,NumberToSkip,Flags,Mode);
end;

function TMongoCollection.FindJSON(
  Criteria: PUTF8Char; const CriteriaParams: array of const;
  const Projection: variant; NumberToReturn, NumberToSkip: integer;
  Flags: TMongoQueryFlags; Mode: TMongoJSONMode): RawUTF8;
begin
  result := FindJSON(BSONVariant(Criteria,[],CriteriaParams),
    Projection,NumberToReturn,NumberToSkip,Flags,Mode);
end;

procedure TMongoCollection.Insert(const Documents: array of variant;
  Flags: TMongoInsertFlags; NoAcknowledge: boolean);
begin
  Database.Client.Connections[0].SendAndFree(TMongoRequestInsert.Create(
    fFullCollectionName,Documents,Flags),NoAcknowledge);
end;

procedure TMongoCollection.Insert(const Documents: TBSONDocument;
  Flags: TMongoInsertFlags; NoAcknowledge: boolean);
begin
  Database.Client.Connections[0].SendAndFree(TMongoRequestInsert.Create(
    fFullCollectionName,Documents,Flags),NoAcknowledge);
end;

procedure TMongoCollection.InsertJSON(const JSONDocuments: array of PUTF8Char;
  Flags: TMongoInsertFlags; NoAcknowledge: boolean);
begin
  Database.Client.Connections[0].SendAndFree(TMongoRequestInsert.Create(
    fFullCollectionName,JSONDocuments,Flags),NoAcknowledge);
end;

function EnsureDocumentHasID(var doc: TDocVariantData; oid: PPVariant;
  DocumentObjectID: PBSONObjectID): boolean;
var ndx: integer;
    id: TBSONObjectID;
    v: PVariant;
begin
  ndx := doc.GetValueIndex('_id',3,true);
  if ndx<0 then begin
    ndx := doc.InternalAdd('_id');
    v := @doc.Values[ndx];
    result := true; // if _id needed to be computed (i.e. save=insert)
  end else begin
    v := @doc.Values[ndx];
    result := PVarData(v)^.VType<=varNull; // _id may be an Int64=TID, not a ObjectID
  end;
  if result then begin
    id.ComputeNew;
    id.ToVariant(v^);
    if DocumentObjectID<>nil then
      DocumentObjectID^ := id;
  end else
    if DocumentObjectID<>nil then
      if not DocumentObjectID^.FromVariant(v^) then
        DocumentObjectID^.Init;
  if oid<>nil then
    oid^ := v;
end;

procedure TMongoCollection.Insert(const Document: RawUTF8;
  const Params: array of const; DocumentObjectID: PBSONObjectID);
var doc: variant;
begin
  _JsonFmt(Document,[],Params,JSON_OPTIONS_FAST,doc);
  EnsureDocumentHasID(TDocVariantData(doc),nil,DocumentObjectID);
  Insert([doc]);
end;

function TMongoCollection.Save(var Document: variant;
  DocumentObjectID: PBSONObjectID): boolean;
var oid: PVariant;
begin
  if not DocVariantType.IsOfType(Document) then
    Document := _JsonFast(VariantSaveMongoJSON(Document,modMongoShell));
  result := EnsureDocumentHasID(_Safe(Document,dvObject)^,@oid,DocumentObjectID);
  if result then
    Insert([Document]) else
    Update(BSONVariant(['_id',oid^]),Document,[mufUpsert])
end;

procedure TMongoCollection.Save(const Document: RawUTF8;
  const Params: array of const; DocumentObjectID: PBSONObjectID);
var doc: variant;
begin
  _JsonFmt(Document,[],Params,JSON_OPTIONS_FAST,doc);
  Save(doc,DocumentObjectID);
end;

procedure TMongoCollection.Update(Query: PUTF8Char;
  const QueryParams: array of const; const Update: RawUTF8;
  const UpdateParams: array of const; Flags: TMongoUpdateFlags);
var quer,upd: variant;
begin
  quer := BSONVariant(Query,[],QueryParams);
  upd := BSONVariant(Update,[],UpdateParams);
  self.Update(quer,upd,Flags);
end;

procedure TMongoCollection.Update(const Query, Update: variant;
  Flags: TMongoUpdateFlags);
begin
  Database.Client.Connections[0].SendAndFree(TMongoRequestUpdate.Create(
    fFullCollectionName,Query,Update,Flags),false);
end;

procedure TMongoCollection.UpdateOne(const _id, UpdatedFields: variant);
begin
  Update(BSONVariant(['_id',_id]),BSONVariant(['$set',UpdatedFields]));
end;

procedure TMongoCollection.Remove(const Query: variant;
  Flags: TMongoDeleteFlags);
begin
  Database.Client.Connections[0].SendAndFree(TMongoRequestDelete.Create(
    fFullCollectionName,Query,Flags),False);
end;

procedure TMongoCollection.RemoveOne(const _id: TBSONObjectID);
begin
  Remove(BSONVariant(['_id',_id.ToVariant]),[mdfSingleRemove]);
end;

procedure TMongoCollection.RemoveOne(const _id: variant);
begin
  Remove(BSONVariant(['_id',_id]),[mdfSingleRemove]);
end;

procedure TMongoCollection.RemoveFmt(Query: PUTF8Char;
  const QueryParams: array of const; Flags: TMongoDeleteFlags);
begin
  Remove(BSONVariant(Query,[],QueryParams),Flags);
end;


{ TDecimal128 }

// see https://github.com/mongodb/libbson/blob/master/src/bson/bson-decimal128.c

procedure TDecimal128.SetZero;
begin
  Bits.lo := 0;
  Bits.hi := BSON_DECIMAL128_HI_INT64POS;
end;

const
  D128: array[TDecimal128SpecialValue] of TDecimal128Bits = (
    // dsvError, dsvValue, dsvNan, dsvZero, dsvPosInf, dsvNegInf, dsvMin, dsvMax
    (lo:0; hi:BSON_DECIMAL128_HI_NAN), (lo:0; hi:BSON_DECIMAL128_HI_NAN),
    (lo:0; hi:BSON_DECIMAL128_HI_NAN), (lo:0; hi:BSON_DECIMAL128_HI_INT64POS),
    (lo:0; hi:$7800000000000000), (lo:0; hi:QWord($f800000000000000)),
    (lo:$378d8e63ffffffff; hi:QWord($dfffed09bead87c0)),
    (lo:$378d8e63ffffffff; hi:$5fffed09bead87c0) );

procedure TDecimal128.SetSpecial(special: TDecimal128SpecialValue);
begin
  Bits := D128[special];
end;

function TDecimal128.IsSpecial: TDecimal128SpecialValue;
begin
  for result := dsvNan to high(D128) do
    if (D128[result].hi=Bits.hi) and (D128[result].lo=Bits.lo) then
      exit;
  result := dsvValue;
end;

procedure TDecimal128.FromInt32(value: integer);
begin
  if value>=0 then begin
    Bits.lo := value;
    Bits.hi := BSON_DECIMAL128_HI_INT64POS;
  end else begin
    Bits.lo := -value;
    Bits.hi := QWord(BSON_DECIMAL128_HI_INT64NEG);
  end;
end;

procedure TDecimal128.FromUInt32(value: cardinal);
begin
  Bits.lo := value;
  Bits.hi := BSON_DECIMAL128_HI_INT64POS;
end;

procedure TDecimal128.FromInt64(value: Int64);
begin
  if value>=0 then begin
    Bits.lo := value;
    Bits.hi := BSON_DECIMAL128_HI_INT64POS;
  end else begin
    Bits.lo := -value;
    Bits.hi := QWord(BSON_DECIMAL128_HI_INT64NEG);
  end;
end;

procedure TDecimal128.FromQWord(value: QWord);
begin
  Bits.lo := value;
  Bits.hi := BSON_DECIMAL128_HI_INT64POS;
end;

function TDecimal128.FromFloat(const value: TSynExtended; precision: integer): boolean;
var tmp: shortstring;
begin
  if (precision<=0) or (precision=DOUBLE_PRECISION) then
    tmp[0] := AnsiChar(DoubleToShort(tmp,value)) else
    tmp[0] := AnsiChar(ExtendedToShort(tmp,value,precision));
  result := true;
  case FloatToShortNan(tmp) of
  fnNan:    SetSpecial(dsvNan);
  fnInf:    SetSpecial(dsvPosInf);
  fnNegInf: SetSpecial(dsvNegInf);
  else result := FromText(@tmp[1],ord(tmp[0]))<>dsvError;
  end;
end;

procedure TDecimal128.FromCurr(const value: Currency);
begin // force exactly 4 decimals
  if value<0 then begin
    Bits.lo := -PInt64(@value)^;
    Bits.hi := QWord(BSON_DECIMAL128_HI_CURRNEG);
  end else begin
    Bits.lo := PInt64(@value)^;
    Bits.hi := BSON_DECIMAL128_HI_CURRPOS;
  end;
end;

function TDecimal128.Equals(const other: TDecimal128): boolean;
begin
  result := (Bits.lo=other.Bits.lo) and (Bits.hi=other.Bits.hi);
end;

function div128bits9digits(var value: THash128Rec): PtrUInt;
var r: QWord;
    i: PtrInt;
begin
  r := 0;
  for i := 0 to high(value.c) do begin
    {$ifdef FPC_32} // circumvent bug at least with FPC 3.2
    Int64Rec(r).Hi := Int64Rec(r).Lo;
    Int64Rec(r).Lo := 0;
    {$else}
    r := r shl 32;    // adjust remainder to match value of next dividend
    {$endif FPC_32}
    inc(r,value.c[i]); // add the divided to _rem
    if r=0 then
      continue;
    value.c[i] := r div 1000000000;
    dec(r,QWord(value.c[i])*1000000000);
  end;
  result := r;
end;

procedure append(var dest: PUTF8Char; var dig: PByte; digits: PtrInt);
  {$ifdef HASINLINE}inline;{$endif}
begin
  if digits>0 then
    repeat
      dest^ := AnsiChar(dig^+ord('0'));
      inc(dig);
      inc(dest);
      dec(digits);
      if digits=0 then
        break;
    until false;
end;

function TDecimal128.ToText(out Buffer: TDecimal128Str): integer;
var dest: PUTF8Char;
    dig: PByte;
    exp, sciexp, signdig, radixpos, j, k: PtrInt;
    combi, biasedexp, signmsb: PtrUInt;
    leastdig, fastdiv: cardinal;
    digbuffer: array[0..35] of byte;
    _128: THash128Rec;
begin
  dest := @Buffer;
  if Bits.h<0 then begin
    dest^ := '-';
    inc(dest);
  end;
  if (Bits.lo=0) and (Bits.hi=0) then begin
    dest^ := '0';
    result := 1;
    exit;
  end;
  combi := (Bits.c[3] shr 26) and $1f;
  if combi shr 3=3 then
    case combi of
    30: begin
      result := AppendRawUTF8ToBuffer(dest,DECIMAL128_SPECIAL_TEXT[dsvPosInf])-PUTF8Char(@Buffer);
      exit;
    end;
    31: begin
      result := AppendRawUTF8ToBuffer(@Buffer,DECIMAL128_SPECIAL_TEXT[dsvNan])-PUTF8Char(@Buffer);
      exit;
    end;
    else begin
      biasedexp := (Bits.c[3] shr 15) and $3fff;
      signmsb := ((Bits.c[3] shr 14) and 1)+8;
    end;
    end
  else begin
    biasedexp := (Bits.c[3] shr 17) and $3fff;
    signmsb := (Bits.c[3] shr 14) and 7;
  end;
  exp := biasedexp-BSON_DECIMAL128_EXPONENT_BIAS;
  _128.c[0] := (Bits.c[3] and $3fff)+((signmsb and $0f)shl 14);
  _128.c[1] := Bits.c[2];
  _128.c[2] := Bits.c[1];
  _128.c[3] := Bits.c[0];
  FillCharFast(digbuffer,sizeof(digbuffer),0);
  dig := @digbuffer;
  if ((_128.lo=0) and (_128.hi=0)) or (_128.c[0]>=1 shl 17) then
    signdig := 1 // non-canonical or zero -> 0
  else begin
    for k := 3 downto 0 do begin
      if (_128.lo=0) and (_128.hi=0) then
        break;
      leastdig := div128bits9digits(_128);
      if leastdig=0 then
        continue;
      for j := 8 downto 0 do begin
        {$ifdef CPU32DELPHI}
        asm // Delphi compiler is not efficient about division
          mov   eax, leastdig
          mov   fastdiv, eax
          mov   edx, 3435973837
          mul   edx
          shr   edx, 3
          mov   leastdig, edx
        end;
        {$else}
        fastdiv := leastdig;
        leastdig := leastdig div 10; // FPC will use reciprocal division
        {$endif CPU32DELPHI}
        digbuffer[k*9+j] := fastdiv-leastdig*10;
        if leastdig=0 then
          break;
      end;
    end;
    signdig := 36; // 4*9 = k*j loops above
    while dig^=0 do begin
      dec(signdig);
      inc(dig);
    end;
  end;
  sciexp := signdig-1+exp;
  if (sciexp<-6) or (exp>0) then begin // scientific format
    dest^ := AnsiChar(dig^+ord('0'));
    inc(dig);
    inc(dest);
    dec(signdig);
    if signdig<>0 then begin
      dest^ := '.';
      inc(dest);
      append(dest,dig,signdig);
    end;
    if sciexp>0 then
      PWord(dest)^ := ord('E')+ord('+')shl 8 else begin
      PWord(dest)^ := ord('E')+ord('-')shl 8;
      sciexp := -sciexp;
    end;
    dest := AppendUInt32ToBuffer(dest+2,sciexp)
  end else begin
    if exp>=0 then // regular format with no decimal place
      append(dest,dig,signdig)
    else begin
      radixpos := signdig+exp;
      if radixpos>0 then // non-zero digits before radix
        append(dest,dig,radixpos)
      else begin
        dest^ := '0'; // leading zero before radix point
        inc(dest);
      end;
      dest^ := '.';   // radix char
      inc(dest);
      while radixpos<0 do begin // leading zeros after radix
        dest^ := '0';
        inc(dest);
        inc(radixpos);
      end;
      append(dest,dig,signdig-radixpos);
    end;
  end;
  result := dest-PUTF8Char(@Buffer);
end;

function TDecimal128.ToText: RawUTF8;
var tmp: TDecimal128Str;
begin
  FastSetString(result,@tmp,ToText(tmp));
end;

procedure TDecimal128.ToText(var result: RawUTF8);
var tmp: TDecimal128Str;
begin
  FastSetString(result,@tmp,ToText(tmp));
end;

procedure TDecimal128.AddText(W: TTextWriter);
var tmp: TDecimal128Str;
begin
  W.AddNoJSONEscape(@tmp,ToText(tmp));
end;

function TDecimal128.ToVariant: variant;
begin
  ToVariant(result);
end;

procedure TDecimal128.ToVariant(out result: variant);
begin
  with TBSONVariantData(result) do begin
    VType := BSONVariantType.VarType;
    VKind := betDecimal128;
    VBlob := nil;
    SetString(RawByteString(VBlob),PAnsiChar(@Bits),sizeof(TDecimal128));
  end;
end;

function TDecimal128.ToFloat: TSynExtended;
var tmp: TDecimal128Str;
begin
  tmp[ToText(tmp)] := #0; // makes ASCIIZ temporary text conversion
  result := GetExtended(@tmp);
end;

function TDecimal128.ToCurr: currency;
begin
  ToCurr(result);
end;

procedure TDecimal128.ToCurr(out result: currency);
var tmp: TDecimal128Str;
    res64: Int64 absolute result;
begin
  if Bits.hi=QWord(BSON_DECIMAL128_HI_CURRNEG) then // was e.g. FromCurr
    res64 := -Bits.lo else
  if Bits.hi=BSON_DECIMAL128_HI_CURRPOS then
    res64 := Bits.lo else begin
    tmp[ToText(tmp)] := #0; // makes ASCIIZ temporary text conversion
    res64 := StrToCurr64(@tmp);
  end;
end;

function TDecimal128.FromText(text: PUTF8Char; textlen: integer): TDecimal128SpecialValue;
var P,PEnd: PUTF8Char;
    c: AnsiChar;
    flags: set of (negative, signed, radix, nonzero);
    digits: array[0..BSON_DECIMAL128_MAX_DIGITS-1] of byte;
    firstnon0, digread, digstored, digcount, radixpos,
    digfirst, diglast, exp, signdig, i: PtrInt;
    signhi, signlo, biasedexp: QWord;
    sign: THash128Rec;
begin
  for result := dsvNan to dsvNegInf do
    if IdemPropNameU(DECIMAL128_SPECIAL_TEXT[result],text,textlen) then begin
      Bits := D128[result];
      exit; // fast recognition of special text values (including '0')
    end;
  Bits := D128[dsvError];
  result := dsvError;
  if (textlen=0) or (text=nil) then
    exit;
  P := text;
  PEnd := text+textlen;
  flags := [];
  if P^ in ['+','-'] then begin
    include(flags,signed);
    if P^='-' then
      include(flags,negative);
    inc(P);
  end;
  digcount := 0;
  digread := 0;
  digstored := 0;
  radixpos := 0;
  firstnon0 := 0;
  exp := 0;
  while P<PEnd do begin
    c := P^;
    case c of
    '.':
      if radix in flags then // duplicated '.'
        exit else begin
        include(flags,radix);
        inc(P);
        continue;
      end;
    '0'..'9':
      if digstored<BSON_DECIMAL128_MAX_DIGITS then
        if (c>'0') or (nonzero in flags) then begin
          if not(nonzero in flags) then begin
            firstnon0 := digread;
            include(flags,nonzero);
          end;
          digits[digstored] := ord(c)-ord('0');
          inc(digstored);
        end;
    'E','e': begin
      inc(P);
      if P>=PEnd then
        exit;
      exp := GetInteger(P,PEnd);
      break;
    end;
    else exit;
    end;
    if nonzero in flags then
      inc(digcount);
    if radix in flags then
      inc(radixpos);
    inc(digread);
    inc(P);
  end;
  if digread=0 then
    exit;
  digfirst := 0;
  if digstored=0 then begin // value is zero
    diglast := 0;
    digits[0] := 0;
    digcount := 1;
    digstored := 1;
    signdig := 0;
  end else begin
    diglast := digstored-1;
    signdig := digcount;
    // handle trailing zeros as non-significant
    while text[firstnon0+signdig-1+ord(radix in flags)+ord(signed in flags)]='0' do
      dec(signdig);
  end;
  if (exp<=radixpos) and (radixpos-exp>1 shl 14) then
    exp := BSON_DECIMAL128_EXPONENT_MIN else
    dec(exp,radixpos);
  while exp>BSON_DECIMAL128_EXPONENT_MAX do begin
    inc(diglast);
    digits[diglast] := 0;
    if diglast-digfirst>BSON_DECIMAL128_MAX_DIGITS then
      if signdig=0 then begin // zero clamping is allowed
        exp := BSON_DECIMAL128_EXPONENT_MAX;
        break;
      end else
        exit; // overflow is not permitted
    dec(exp);
  end;
  while (exp<BSON_DECIMAL128_EXPONENT_MIN) or (digstored<digcount) do begin
    if diglast=0 then
      if signdig=0 then begin // zero clamping
        exp := BSON_DECIMAL128_EXPONENT_MIN;
        break;
      end else
        exit; // overflow
    if digstored<digcount then
      if (text[digcount-1+ord(signed in flags)+ord(radix in flags)]<>'0') and
         (signdig<>0) then
        exit else // overflow
        dec(digcount) else // adjust to non stored digits
      if digits[diglast]<>0 then
        exit else // inexact rounding
        dec(diglast); // adjust to round
    if exp<BSON_DECIMAL128_EXPONENT_MAX then
      inc(exp) else
      exit;
  end;
  if diglast-digfirst+1<signdig then
    if text[firstnon0+diglast+ord(signed in flags)+ord(radix in flags)]<>'0' then
      exit; // inexact rouding
  signhi := 0;
  signlo := 0;
  if signdig<>0 then // if not zero
    if diglast-digfirst<17 then
      {$ifdef CPU32DELPHI} // use "shl" under x86 to avoid slower "call _llmul"
      for i := digfirst to diglast do
        inc(signlo,signlo+signlo shl 3+digits[i]) else begin
      for i := digfirst to diglast-17 do
        inc(signhi,signhi+signhi shl 3+digits[i]);
      for i := diglast-16 to diglast do
        inc(signlo,signlo+signlo shl 3+digits[i]);
      {$else}
      for i := digfirst to diglast do
        signlo := signlo*10+digits[i] else begin
      for i := digfirst to diglast-17 do
        signhi := signhi*10+digits[i];
      for i := diglast-16 to diglast do
        signlo := signlo*10+digits[i];
      {$endif}
    end;
  if signhi=0 then begin
    sign.L := signlo;
    sign.H := 0;
  end else begin
    mul64x64(signhi,100000000000000000,sign);
    inc(sign.L,signlo);
    {$ifdef FPC}
    if sign.L<signlo then
    {$else} // manual QWord processs (for oldest Delphi compilers)
    if (sign.c1<TQWordRec(signlo).H) or
       ((sign.c1=TQWordRec(signlo).H) and (sign.c0<TQWordRec(signlo).L)) then
    {$endif}
      inc(sign.H);
  end;
  biasedexp := exp+BSON_DECIMAL128_EXPONENT_BIAS;
  if (sign.H shr 49)and 1<>0 then
    Bits.hi := (QWord(3) shl 61) or ((biasedexp and $3fff)shl 47) or
      (sign.H and $7fffffffffff) else
    Bits.hi := ((biasedexp and $3fff)shl 49) or
      (sign.H and $1ffffffffffff);
  Bits.lo := sign.L;
  if negative in flags then
    Bits.c[3] := Bits.c[3] or $80000000;
  result := dsvValue;
end;

function TDecimal128.FromText(const text: RawUTF8): TDecimal128SpecialValue;
begin
  result := FromText(pointer(text),length(text));
end;

function TDecimal128.FromVariant(const value: variant): boolean;
var txt: RawUTF8;
    wasString: boolean;
    bson: TBSONVariantData absolute value;
    v64: Int64;
begin
  if bson.VType=varByRef or varVariant then begin
    result := FromVariant(PVariant(TVarData(value).VPointer)^);
    exit;
  end;
  if (bson.VType=BSONVariantType.VarType) and (bson.VKind=betDecimal128) then
    Bits := PDecimal128(bson.VBlob)^.Bits else
  if bson.VType=varWord64 then
    FromQWord(TVarData(Value).VInt64) else
  if VariantToInt64(value,v64) then
    FromInt64(v64) else
  if bson.VType=varCurrency then
    FromCurr(TVarData(value).VCurrency) else begin
    VariantToUTF8(value,txt,wasString);
    result := FromText(txt)<>dsvError;
    exit;
  end;
  result := true;
end;

initialization
  Assert(sizeof(TDecimal128)=16);
  Assert(ord(betEof)=$00);
  Assert(ord(betInt64)=$12);
  Assert(ord(betDecimal128)=$13);
  Assert(ord(bbtGeneric)=$00);
  Assert(ord(bbtMD5)=$05);
  Assert(ord(bbtUser)=$80);
  Assert(sizeof(TBSONObjectID)=12);
  Assert(sizeof(TBSONVariantData)=sizeof(variant));
  Assert(sizeof(TMongoReplyHeader)=36);
  BSONVariantType := SynRegisterCustomVariantType(TBSONVariant) as TBSONVariant;
  InitBSONObjectIDComputeNew;

finalization
  DeleteCriticalSection(GlobalBSONObjectID.Section);
end.



