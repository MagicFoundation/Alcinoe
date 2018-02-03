program UDExtract;

{$APPTYPE CONSOLE}

// Application to convert a Unicode database file into a resource script compilable
// to a resource file. For usage see procedure PrintUsage.

uses
  Classes,
  SysUtils,
  JclSysUtils,
  JclCompression,
  BZip2,
  ZLibh,
  JclBase,
  JclAnsiStrings,
  JclLogic,
  JclStrings,
  JclUnicode,
  JclStreams;

type
  TDecompositions = array of Cardinal;

  TDecomposition = record
    Code: Cardinal;
    Tag: TCompatibilityFormattingTag; 
    Decompositions: TDecompositions;
  end;

  // collect of case mappings for each code point which is cased
  TCase = record
    Code: Cardinal;
    Fold,               // normalized case for case independent string comparison (e.g. for "ß" this is "ss")
    Lower,              // lower case (e.g. for "ß" this is "ß")
    Title,              // tile case (used mainly for compatiblity, ligatures etc., e.g. for "ß" this is "Ss")
    Upper: TUCS4Array;  // upper cae (e.g. for "ß" this is "SS")
  end;

  // structures for handling numbers
  TCodeIndex = record
    Code,
    Index: Cardinal;
  end;

  TNumber = record
    Numerator,
    Denominator: Integer;
  end;

  // start and stop of a range of code points
  TCharacterSet = array [0..$1000000 div BitsPerByte] of Byte;

  // start and stop of a range of code points
  TRange = record
    Start,
    Stop: Cardinal;
  end;

  TRangeArray = array of TRange;

  CategoryString = record
    Name: AnsiString;
    Category: TCharacterCategory;
  end;

const
  // List of categories expected to be found in the Unicode Character Database
  // including some implementation specific properties.
  // Note: there are multiple definitions which describe the same property (because they are used in the general
  //       categories as well as bidirectional categories (while we store both types as one).
  //       These are:
  //       - Mn, NSM for non-spacing mark
  //       - Zp, B for paragraph separator
  CategoriesStrings: array[0..87] of CategoryString = (
    // normative categories
    (Name: 'Lu';  Category: ccLetterUppercase),           // letter, upper case
    (Name: 'Ll';  Category: ccLetterLowercase),           // letter, lower case
    (Name: 'Lt';  Category: ccLetterTitlecase),           // letter, title case
    (Name: 'Mn';  Category: ccMarkNonSpacing),            // mark, non spacing
    (Name: 'NSM'; Category: ccMarkNonSpacing),
    (Name: 'Mc';  Category: ccMarkSpacingCombining),      // mark, spacing combining
    (Name: 'Me';  Category: ccMarkEnclosing),             // mark, enclosing
    (Name: 'Nd';  Category: ccNumberDecimalDigit),        // number, decimal digit
    (Name: 'Nl';  Category: ccNumberLetter),              // number, letter
    (Name: 'No';  Category: ccNumberOther),               // number, other
    (Name: 'Zs';  Category: ccSeparatorSpace),            // separator, space
    (Name: 'Zl';  Category: ccSeparatorLine),             // separator, line
    (Name: 'Zp';  Category: ccSeparatorParagraph),        // separator, paragraph
    (Name: 'B';   Category: ccSeparatorParagraph),
    (Name: 'Cc';  Category: ccOtherControl),              // other, control
    (Name: 'Cf';  Category: ccOtherFormat),               // other, format
    (Name: 'Cs';  Category: ccOtherSurrogate),            // other, surrogate
    (Name: 'Co';  Category: ccOtherPrivate),              // other, private use
    (Name: 'Cn';  Category: ccOtherUnassigned),           // other, not assigned
    // informative categories
    (Name: 'Lm';  Category: ccLetterModifier),            // letter, modifier
    (Name: 'Lo';  Category: ccLetterOther),               // letter, other
    (Name: 'Pc';  Category: ccPunctuationConnector),      // punctuation, connector
    (Name: 'Pd';  Category: ccPunctuationDash),           // punctuation, dash
    (Name: 'Dash'; Category: ccPunctuationDash),
    (Name: 'Ps';  Category: ccPunctuationOpen),           // punctuation, open
    (Name: 'Pe';  Category: ccPunctuationClose),          // punctuation, close
    (Name: 'Pi';  Category: ccPunctuationInitialQuote),   // punctuation, initial quote
    (Name: 'Pf';  Category: ccPunctuationFinalQuote),     // punctuation, final quote
    (Name: 'Po';  Category: ccPunctuationOther),          // punctuation, other
    (Name: 'Sm';  Category: ccSymbolMath),                // symbol, math
    (Name: 'Sc';  Category: ccSymbolCurrency),            // symbol, currency
    (Name: 'Sk';  Category: ccSymbolModifier),            // symbol, modifier
    (Name: 'So';  Category: ccSymbolOther),               // symbol, other
    // bidirectional categories
    (Name: 'L';   Category: ccLeftToRight),               // left-to-right
    (Name: 'LRE'; Category: ccLeftToRightEmbedding),      // left-to-right embedding
    (Name: 'LRO'; Category: ccLeftToRightOverride),       // left-to-right override
    (Name: 'R';   Category: ccRightToLeft),               // right-to-left
    (Name: 'AL';  Category: ccRightToLeftArabic),         // right-to-left arabic
    (Name: 'RLE'; Category: ccRightToLeftEmbedding),      // right-to-left embedding
    (Name: 'RLO'; Category: ccRightToLeftOverride),       // right-to-left override
    (Name: 'PDF'; Category: ccPopDirectionalFormat),      // pop directional format
    (Name: 'EN';  Category: ccEuropeanNumber),            // european number
    (Name: 'ES';  Category: ccEuropeanNumberSeparator),   // european number separator
    (Name: 'ET';  Category: ccEuropeanNumberTerminator),  // european number terminator
    (Name: 'AN';  Category: ccArabicNumber),              // arabic number
    (Name: 'CS';  Category: ccCommonNumberSeparator),     // common number separator
    (Name: 'BN';  Category: ccBoundaryNeutral),           // boundary neutral
    (Name: 'S';   Category: ccSegmentSeparator),          // segment separator
    (Name: 'WS';  Category: ccWhiteSpace),                // white space
    (Name: 'White_Space'; Category: ccWhiteSpace),
    (Name: 'ON';  Category: ccOtherNeutrals),             // other neutrals
    // self defined categories, they do not appear in the Unicode data file
    (Name: 'Cm';  Category: ccComposed),                  // composed (can be decomposed)
    (Name: 'Nb';  Category: ccNonBreaking),               // non-breaking
    (Name: 'Sy';  Category: ccSymmetric),                 // symmetric (has left and right forms)
    (Name: 'Hd';  Category: ccHexDigit),                  // hex digit
    (Name: 'Hex_Digit'; Category: ccHexDigit),
    (Name: 'Qm';  Category: ccQuotationMark),             // quote marks
    (Name: 'Quotation_Mark'; Category: ccQuotationMark),
    (Name: 'Mr';  Category: ccMirroring),                 // mirroring
    (Name: 'Cp';  Category: ccAssigned),                  // assigned character (there is a definition in the Unicode standard)
    //'Luu' // letter unique upper case
    (Name: 'Bidi_Control'; Category: ccBidiControl),
    (Name: 'Join_Control'; Category: ccJoinControl),
    (Name: 'Hyphen'; Category: ccHyphen),
    (Name: 'Terminal_Punctuation'; Category: ccTerminalPunctuation),
    (Name: 'Other_Math'; Category: ccOtherMath),
    (Name: 'ASCII_Hex_Digit'; Category: ccASCIIHexDigit),
    (Name: 'Other_Alphabetic'; Category: ccOtherAlphabetic),
    (Name: 'Ideographic'; Category: ccIdeographic),
    (Name: 'Diacritic'; Category: ccDiacritic),
    (Name: 'Extender'; Category: ccExtender),
    (Name: 'Other_Lowercase'; Category: ccOtherLowercase),
    (Name: 'Other_Uppercase'; Category: ccOtherUppercase),
    (Name: 'Noncharacter_Code_Point'; Category: ccNonCharacterCodePoint),
    (Name: 'Other_Grapheme_Extend'; Category: ccOtherGraphemeExtend),
    (Name: 'IDS_Binary_Operator'; Category: ccIDSBinaryOperator),
    (Name: 'IDS_Trinary_Operator'; Category: ccIDSTrinaryOperator),
    (Name: 'Radical'; Category: ccRadical),
    (Name: 'Unified_Ideograph'; Category: ccUnifiedIdeograph),
    (Name: 'Other_Default_Ignorable_Code_Point'; Category: ccOtherDefaultIgnorableCodePoint),
    (Name: 'Deprecated'; Category: ccDeprecated),
    (Name: 'Soft_Dotted'; Category: ccSoftDotted),
    (Name: 'Logical_Order_Exception'; Category: ccLogicalOrderException),
    (Name: 'Other_ID_Start'; Category: ccOtherIDStart),
    (Name: 'Other_ID_Continue'; Category: ccOtherIDContinue),
    (Name: 'STerm'; Category: ccSTerm),
    (Name: 'Variation_Selector'; Category: ccVariationSelector),
    (Name: 'Pattern_White_Space'; Category: ccPatternWhiteSpace),
    (Name: 'Pattern_Syntax'; Category: ccPatternSyntax)
    );

const
  DecompositionTags: array [TCompatibilityFormattingTag] of string =
    ('',            // cftCanonical
     '<font>',      // cftFont
     '<noBreak>',   // cftNoBreak
     '<initial>',   // cftInitial
     '<medial>',    // cftMedial
     '<final>',     // cftFinal
     '<isolated>',  // cftIsolated
     '<circle>',    // cftCircle
     '<super>',     // cftSuper
     '<sub>',       // cftSub
     '<vertical>',  // cftVertical
     '<wide>',      // cftWide
     '<narrow>',    // cftNarrow
     '<small>',     // cftSmall
     '<square>',    // cftSquare
     '<fraction>',  // cftFraction
     '<compat>');   // cftCompat

var
  SourceFileName,
  SpecialCasingFileName,
  CaseFoldingFileName,
  DerivedNormalizationPropsFileName,
  PropListFileName,
  TargetFileName: TFileName;
  Verbose: Boolean;
  ZLibCompress: Boolean;
  BZipCompress: Boolean;

  // character category ranges
  Categories: array[TCharacterCategory] of TCharacterSet;
  // canonical combining classes
  CCCs: array[Byte] of TCharacterSet;
  // list of decomposition
  Decompositions: array of TDecomposition;
  // array to hold the number equivalents for specific codes (sorted by code)
  NumberCodes: array of TCodeIndex;
  // array of numbers used in NumberCodes
  Numbers: array of TNumber;
  // array for all case mappings (including 1 to many casing if a special casing source file was given)
  CaseMapping: array of TCase;
  // array of compositions (somehow the same as Decompositions except sorted by decompositions and removed elements)
  Compositions: array of TDecomposition;
  // array of composition exception ranges
  CompositionExceptions: TCharacterSet;

//----------------------------------------------------------------------------------------------------------------------

procedure FatalError(const S: string);

begin
  if not Verbose then
  begin
    Writeln;
    Writeln('[Fatal error] ' + S);
  end;
  ExitCode := 4;
  Abort;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Warning(const S: string);

begin
  if not Verbose then
  begin
    Writeln;
    Writeln('[Warning] ' + S);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function IsHexDigit(C: Char): Boolean;

begin
  Result := CharIsHexDigit(C);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SetCharacter(var CharacterSet: TCharacterSet; Code: Cardinal);
begin
  SetBitBuffer(CharacterSet[0], Code);
end;

//----------------------------------------------------------------------------------------------------------------------

function TestCharacter(const CharacterSet: TCharacterSet; Code: Cardinal): Boolean;
begin
  Result := TestBitBuffer(CharacterSet[0], Code);
end;

//----------------------------------------------------------------------------------------------------------------------

function FindNextCharacterRange(const CharacterSet: TCharacterSet; var Start, Stop: Cardinal): Boolean;
var
  ByteIndex: Cardinal;
begin
  ByteIndex := Start div BitsPerByte;
  if (ByteIndex < ($1000000 div BitsPerByte)) and (CharacterSet[ByteIndex] = 0) then
  begin
    while (ByteIndex < ($1000000 div BitsPerByte)) and (CharacterSet[ByteIndex] = 0) do
      Inc(ByteIndex);
    Start := ByteIndex * BitsPerByte;
  end;

  while (Start < $1000000) and not TestBitBuffer(CharacterSet[0], Start) do
    Inc(Start);

  if Start < $1000000 then
  begin
    Result := True;
    Stop := Start;

    ByteIndex := Stop div BitsPerByte;
    if (ByteIndex < ($1000000 div BitsPerByte)) and (CharacterSet[ByteIndex] = $FF) then
    begin
      while (ByteIndex < ($1000000 div BitsPerByte)) and (CharacterSet[ByteIndex] = $FF) do
        Inc(ByteIndex);
      Stop := ByteIndex * BitsPerByte;
    end;

    while (Stop < $1000000) and TestBitBuffer(CharacterSet[0], Stop) do
      Inc(Stop);
    if Stop <= $1000000 then
      Dec(Stop);
  end
  else
    Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function FindCharacterRanges(const CharacterSet: TCharacterSet): TRangeArray;

var
  Capacity, Index: Integer;
  Start, Stop: Cardinal;

begin
  Capacity := 0;
  Index := 0;
  Start := 0;
  Stop := 0;
  while FindNextCharacterRange(CharacterSet, Start, Stop) do
  begin
    if Index >= Capacity then
    begin
      Inc(Capacity, 64);
      SetLength(Result, Capacity);
    end;
    Result[Index].Start := Start;
    Result[Index].Stop := Stop;
    Start := Stop + 1;
    Inc(Index);
  end;
  SetLength(Result, Index);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddRangeToCategories(Start, Stop: Cardinal; Category: TCharacterCategory); overload;
var
  Code: Integer;
begin
  for Code := Start to Stop do
    SetCharacter(Categories[Category], Code);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddRangeToCategories(Start, Stop: Cardinal; CategoryID: AnsiString); overload;

// Adds a range of code points to the categories structure.

var
  Index: Integer;

begin
  // find category
  for Index := Low(CategoriesStrings) to High(CategoriesStrings) do
    if CategoriesStrings[Index].Name = CategoryID then
    begin
      AddRangeToCategories(Start, Stop, CategoriesStrings[Index].Category);
      Exit;
    end;
  FatalError('No unicode category for ID "' + CategoryID + '"');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddToCategories(Code: Cardinal; Category: TCharacterCategory); overload;
begin
  SetCharacter(Categories[Category], Code);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddToCategories(Code: Cardinal; CategoryID: AnsiString); overload;

// Adds a range of code points to the categories structure.

var
  Index: Integer;

begin
  // find category
  for Index := Low(CategoriesStrings) to High(CategoriesStrings) do
    if CategoriesStrings[Index].Name = CategoryID then
    begin
      AddToCategories(Code, CategoriesStrings[Index].Category);
      Exit;
    end;
  FatalError('No unicode category for ID "' + CategoryID + '"');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddCanonicalCombiningClass(Code, CCClass: Cardinal);
begin
  // most of the code points have a combining class of 0 (so to speak the default class)
  // hence we don't need to store them
  if CCClass > 0 then
    SetCharacter(CCCs[CCClass], Code);
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeNumber(Num, Denom: Integer): Integer;

// adds a number if it does not already exist and returns its index value

var
  I: Integer;

begin
  Result := -1;
  // determine if the number already exists
  for I := 0 to  High(Numbers) do
    if (Numbers[I].Numerator = Num) and (Numbers[I].Denominator = Denom) then
    begin
      Result := I;
      Break;
    end;

  if Result = -1 then
  begin
    Result := Length(Numbers);
    SetLength(Numbers, Result + 1);

    Numbers[Result].Numerator := Num;
    Numbers[Result].Denominator := Denom;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddNumber(Code: Cardinal; Num, Denom: Integer);

var
  I, J: Integer;

begin
  // Insert the Code in order.
  I := 0;
  J := Length(NumberCodes);
  while (I < J) and (Code > NumberCodes[I].Code) do
    Inc(I);

  // Handle the case of the codes matching and simply replace the number that was there before.
  if (I < J) and (Code = NumberCodes[I].Code) then
    NumberCodes[I].Index := MakeNumber(Num, Denom)
  else
  begin
    // Resize the array if necessary.
    SetLength(NumberCodes, J + 1);

    // Shift things around to insert the Code if necessary.
    if I < J then
    begin
      Move(NumberCodes[I], NumberCodes[I + 1], (J - I) * SizeOf(TCodeIndex));
      FillChar(NumberCodes[I], SizeOf(TCodeIndex), 0);
    end;
    NumberCodes[I].Code := Code;
    NumberCodes[I].Index := MakeNumber(Num, Denom);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddDecomposition(Code: Cardinal; Tag: TCompatibilityFormattingTag; Decomposition: TDecompositions);

var
  I, J: Integer;

begin
  AddToCategories(Code, ccComposed);

  // locate the insertion point for the code
  I := 0;
  J := Length(Decompositions);
  while (I < J) and (Code > Decompositions[I].Code) do
    Inc(I);

  if (I = J) or (Decompositions[I].Code <> Code) then
  begin
    // allocate space for a new decomposition
    SetLength(Decompositions, J + 1);

    if I < J then
    begin
      // shift the Decompositions up by one if the codes don't match
      Move(Decompositions[I], Decompositions[I + 1], (J - I) * SizeOf(TDecomposition));
      FillChar(Decompositions[I], SizeOf(TDecomposition), 0);
    end;
  end;

  // insert or replace a decomposition
  if Length(Decompositions[I].Decompositions) <> Length(Decomposition) then
    SetLength(Decompositions[I].Decompositions, Length(Decomposition));

  Decompositions[I].Code := Code;
  Decompositions[I].Tag := Tag;
  Move(Decomposition[0], Decompositions[I].Decompositions[0], Length(Decomposition) * SizeOf(Cardinal));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddRangeToCompositionExclusions(Start, Stop: Cardinal);
var
  Code: Integer;
begin
  for Code := Start to Stop do
    SetCharacter(CompositionExceptions, Code);
end;

//----------------------------------------------------------------------------------------------------------------------

function FindOrAddCaseEntry(Code: Cardinal): Integer;

// Used to look up the given code in the case mapping array. If no entry with the given code
// exists then it is added implicitely.

var
  J: Integer;

begin
  Result := 0;
  J := Length(CaseMapping);
  while (Result < J) and (CaseMapping[Result].Code < Code) do
    Inc(Result);

  // this code is not yet in the case mapping table
  if (Result = J) or (CaseMapping[Result].Code <> Code) then
  begin
    SetLength(CaseMapping, J + 1);

    // locate the insertion point
    Result := 0;
    while (Result < J) and (Code > CaseMapping[Result].Code) do
      Inc(Result);
    if Result < J then
    begin
      // shift the array up by one
      Move(CaseMapping[Result], CaseMapping[Result + 1], (J - Result) * SizeOf(TCase));
      FillChar(CaseMapping[Result], SizeOf(TCase), 0);
    end;
    Casemapping[Result].Code := Code;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddFoldCase(Code: Cardinal; FoldMapping: TUCS4Array);

var
  I: Integer;

begin
  I := FindOrAddCaseEntry(Code);
  if Length(CaseMapping[I].Fold) = 0 then
    CaseMapping[I].Fold := Copy(FoldMapping, 0, Length(FoldMapping))
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddLowerCase(Code: Cardinal; Lower: TUCS4Array);

var
  I: Integer;

begin
  I := FindOrAddCaseEntry(Code);
  if Length(CaseMapping[I].Lower) = 0 then
    CaseMapping[I].Lower := Copy(Lower, 0, Length(Lower))
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddUpperCase(Code: Cardinal; Upper: TUCS4Array);

var
  I: Integer;

begin
  I := FindOrAddCaseEntry(Code);
  if Length(CaseMapping[I].Upper) = 0 then
    CaseMapping[I].Upper := Copy(Upper, 0, Length(Upper))
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddTitleCase(Code: Cardinal; Title: TUCS4Array);

var
  I: Integer;

begin
  I := FindOrAddCaseEntry(Code);
  if Length(CaseMapping[I].Title) = 0 then
    CaseMapping[I].Title := Copy(Title, 0, Length(Title))
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SplitLine(const Line: string; Elements: TStringList);

// splits the given string into parts which are separated by semicolon and fills Elements
// with the partial strings

var
  Head,
  Tail: PChar;
  S: string;
  
begin
  Elements.Clear;
  Head := PChar(Line);
  while Head^ <> #0 do
  begin
    Tail := Head;
    // look for next semicolon or string end (or comment identifier)
    while (Tail^ <> ';') and (Tail^ <> '#') and (Tail^ <> #0) do
      Inc(Tail);
    SetString(S, Head, Tail - Head);
    Elements.Add(Trim(S));
    // ignore all characters in a comment 
    if (Tail^ = '#') or (Tail^ = #0) then
      Break;
    Head := Tail + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SplitCodes(const Line: string; var Elements: TUCS4Array);

// splits S, which may contain space delimited hex strings, into single parts
// and fills Elements

var
  Head,
  Tail: PChar;
  S: string;
  I: Integer;

begin
  Elements := nil;
  Head := PChar(Line);
  while Head^ <> #0 do
  begin
    Tail := Head;
    while IsHexDigit(Tail^) do
      Inc(Tail);
    SetString(S, Head, Tail - Head);
    if Length(S) > 0 then
    begin
      I := Length(Elements);
      SetLength(Elements, I + 1);
      Elements[I] := StrToInt('$' + S);
    end;
    // skip spaces
    while Tail^ = ' ' do
      Inc(Tail);
    if Tail^ = #0 then
     Break;
    Head := Tail;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ParseData;

// ParseData takes the source file and extracts all relevant data into internal structures to be
// used when creating the resource script.

var
  Lines,
  Line,
  SymCharacters,
  DecompositionsStr,
  NumberStr: TStringList;
  I, J, SymIndex: Integer;
  RangePending: Boolean;
  StartCode,
  EndCode: Cardinal;
  K, Tag: TCompatibilityFormattingTag;
  Name, SymName: string;
  Decompositions: TDecompositions;

  // number representation
  Nominator,
  Denominator: Integer;

  // case mapping
  AMapping: TUCS4Array;
  
begin
  Lines := nil;
  SymCharacters := nil;
  Line := nil;
  DecompositionsStr := nil;
  NumberStr := nil;
  try
    Lines := TStringList.Create;
    SymCharacters := TStringList.Create;
    Line := TStringList.Create;
    DecompositionsStr := TStringList.Create;
    NumberStr := TStringList.Create;

    // Unicode data files are about 600K in size, so don't hesitate and load them in one rush.
    Lines.LoadFromFile(SourceFileName);

    // Go for each line, organization is one line for a code point or two consecutive lines
    // for a range of code points.
    RangePending := False;
    StartCode := 0;
    for I := 0 to Lines.Count - 1 do
    begin
      SplitLine(Lines[I], Line);
      // continue only if the line is not empty
      if Line.Count > 1 then
      begin
        Name := UpperCase(Line[1]);
        // Line contains now up to 15 entries, starting with the code point value
        if RangePending then
        begin
          // last line was a range start, so this one must be the range end
          if Pos(', LAST>', Name) = 0 then
            FatalError(Format('Range end expected in line %d.', [I + 1]));
          EndCode := StrToInt('$' + Line[0]);

          // register general category
          AddRangeToCategories(StartCode, EndCode, AnsiString(Line[2]));

          // register bidirectional category
          AddRangeToCategories(StartCode, EndCode, AnsiString(Line[4]));

          // mark the range as containing assigned code points
          AddRangeToCategories(StartCode, EndCode, ccAssigned);
          RangePending := False;
        end
        else
        begin
          StartCode := StrToInt('$' + Line[0]);
          // check for the start of a range
          if Pos(', FIRST>', Name) > 0 then
            RangePending := True
          else
          begin
            // normal case, one code point must be parsed

            // 1) categorize code point as being assinged
            AddToCategories(StartCode, ccAssigned);

            // 2) find symmetric shapping characters
            // replace LEFT by RIGHT and vice-versa
            SymName := StringReplace(Name, 'LEFT', 'LLEEFFTT', [rfReplaceAll]);
            SymName := StringReplace(SymName, 'RIGHT', 'LEFT', [rfReplaceAll]);
            SymName := StringReplace(SymName, 'LLEEFFTT', 'RIGHT', [rfReplaceAll]);
            if Name <> SymName then
            begin
              SymIndex := SymCharacters.IndexOf(SymName);
              if SymIndex >= 0 then
              begin
                AddToCategories(StartCode, ccSymmetric);
                AddToCategories(Cardinal(SymCharacters.Objects[SymIndex]), ccSymmetric);
              end
              else
                SymCharacters.AddObject(Name, TObject(StartCode));
            end;

            if Line.Count < 3 then
              Continue;
            // 3) categorize the general character class
            AddToCategories(StartCode, AnsiString(Line[2]));

            if Line.Count < 4 then
              Continue;
            // 4) register canonical combining class
            AddCanonicalCombiningClass(StartCode, StrToInt(Line[3]));

            if Line.Count < 5 then
              Continue;
            // 5) categorize the bidirectional character class
            AddToCategories(StartCode, AnsiString(Line[4]));

            if Line.Count < 6 then
              Continue;
            // 6) if the character can be decomposed then keep its decomposed parts
            //    and add it to the can-be-decomposed category
            StrToStrings(Line[5], NativeSpace, DecompositionsStr, False);
            Tag := cftCanonical;
            if (DecompositionsStr.Count > 0) and (Pos('<', DecompositionsStr.Strings[0]) > 0) then
            begin
              for K := Low(DecompositionTags) to High(DecompositionTags) do
              begin
                if DecompositionTags[K] = DecompositionsStr.Strings[0] then
                begin
                  Tag := K;
                  Break;
                end;
              end;
              if Tag = cftCanonical then
                FatalError('Unknown decomposition tag ' + DecompositionsStr.Strings[0]);
              if Tag = cftNoBreak then
                AddToCategories(StartCode, ccNonBreaking);
              DecompositionsStr.Delete(0);
            end;
            if (DecompositionsStr.Count > 0) and (Pos('<', DecompositionsStr.Strings[0]) = 0) then
            begin
              // consider only canonical decomposition mappings
              SetLength(Decompositions, DecompositionsStr.Count);
              for J := 0 to DecompositionsStr.Count - 1 do
                Decompositions[J] := StrToInt('$' + DecompositionsStr.Strings[J]);

              // If there is more than one code in the temporary decomposition
              // array then add the character with its decomposition.
              // (outchy) latest unicode data have aliases to link items having the same decompositions
              //if DecompTempSize > 1 then
              AddDecomposition(StartCode, Tag, Decompositions);
            end;

            if Line.Count < 9 then
              Break;
            // 7) examine if there is a numeric representation of this code
            StrToStrings(Line[8], '/', NumberStr, False);
            if NumberStr.Count = 1 then
            begin
              Nominator := StrToInt(NumberStr.Strings[0]);
              Denominator := 1;
              AddNumber(StartCode, Nominator, Denominator);
            end
            else
            if NumberStr.Count = 2 then
            begin
              Nominator := StrToInt(NumberStr.Strings[0]);
              Denominator := StrToInt(NumberStr.Strings[1]);
              AddNumber(StartCode, Nominator, Denominator);
            end
            else
            if NumberStr.Count <> 0 then
              FatalError('Unknown number ' + Line[8]);

            if Line.Count < 10 then
              Continue;
            // 8) read mirrored character
            SymName := Line[9];
            if SymName = 'Y' then
              AddToCategories(StartCode, ccMirroring)
            else
            if SymName <> 'N' then
              FatalError('Unknown mirroring character');

            if Line.Count < 13 then
              Continue;
            SetLength(AMapping, 1);
            // 9) read simple upper case mapping (only 1 to 1 mappings)
            if Length(Line[12]) > 0 then
            begin
              AMapping[0] := StrToInt('$' + Line[12]);
              AddUpperCase(StartCode, AMapping);
            end;

            if Line.Count < 14 then
              Continue;
            // 10) read simple lower case mapping
            if Length(Line[13]) > 0 then
            begin
              AMapping[0] := StrToInt('$' + Line[13]);
              AddLowerCase(StartCode, AMapping);
            end;

            if Line.Count < 15 then
              Continue;
            // 11) read title case mapping
            if Length(Line[14]) > 0 then
            begin
              AMapping[0] := StrToInt('$' + Line[14]);
              AddTitleCase(StartCode, AMapping);
            end;
          end;
        end;
      end;
      if not Verbose then
        Write(Format(#13'  %d%% done', [Round(100 * I / Lines.Count)]));
    end;
  finally
    Lines.Free;
    Line.Free;
    SymCharacters.Free;
    DecompositionsStr.Free;
    NumberStr.Free;
  end;
  if not Verbose then
    Writeln;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ParseSpecialCasing;

// One-to-many case mappings are given by a separate file which is in a different format
// than the Unicode data file. This procedure parses this file and adds those extended mappings
// to the internal array.

var
  Lines,
  Line: TStringList;
  I: Integer;
  Code: Cardinal;

  AMapping: TUCS4Array;

begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(SpecialCasingFileName);
    Line := TStringList.Create;
    try
      for I := 0 to Lines.Count - 1 do
      begin
        SplitLine(Lines[I], Line);
        // continue only if the line is not empty
        if (Line.Count > 0) and (Length(Line[0]) > 0) then
        begin
          Code := StrToInt('$' + Line[0]);
          // extract lower case
          if Length(Line[1]) > 0 then
          begin
            SplitCodes(Line[1], AMapping);
            AddLowerCase(Code, AMapping);
          end;
          // extract title case
          if Length(Line[2]) > 0 then
          begin
            SplitCodes(Line[2], AMapping);
            AddTitleCase(Code, AMapping);
          end;
          // extract upper case
          if Length(Line[3]) > 0 then
          begin
            SplitCodes(Line[3], AMapping);
            AddUpperCase(Code, AMapping);
          end;
        end;
        if not Verbose then
          Write(Format(#13'  %d%% done', [Round(100 * I / Lines.Count)]));
      end;
    finally
      Line.Free;
    end;
  finally
    Lines.Free;
  end;
  if not Verbose then
    Writeln;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ParseCaseFolding;

// Casefolding data is given by yet another optional file. Usually case insensitive string comparisons
// are done by converting the strings to lower case and compare them, but in some cases
// this is not enough. We only add those special cases to our internal casing array.

var
 Lines,
 Line: TStringList;
 I: Integer;
 Code: Cardinal;

 AMapping: TUCS4Array;

begin
 Lines := TStringList.Create;
 try
   Lines.LoadFromFile(CaseFoldingFileName);
   Line := TStringList.Create;
   try
     for I := 0 to Lines.Count - 1 do
     begin
       // Layout of one line is:
       // <code>; <status>; <mapping>; # <name>
       // where status is either "L" describing a normal lowered letter
       // and "E" for exceptions (only the second is read)
       SplitLine(Lines[I], Line);
       // continue only if the line is not empty
       if (Line.Count > 0) and (Length(Line[0]) > 0) then
       begin
         // the code currently being under consideration
         Code := StrToInt('$' + Line[0]);
         // type of mapping
         if ((Line[1] = 'C') or (Line[1] = 'F')) and (Length(Line[2]) > 0) then
         begin
           SplitCodes(Line[2], AMapping);
           AddFoldCase(Code, AMapping);
         end;
       end;
       if not Verbose then
         Write(Format(#13'  %d%% done', [Round(100 * I / Lines.Count)]));
     end;
   finally
     Line.Free;
   end;
 finally
   Lines.Free;
 end;
 if not Verbose then
   Writeln;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ParseDerivedNormalizationProps;

// parse DerivedNormalizationProps looking for composition exclusions

var
 Lines,
 Line: TStringList;
 I, SeparatorPos: Integer;
 Start, Stop: Cardinal;

begin
  Lines := TStringList.Create;
 try
   Lines.LoadFromFile(DerivedNormalizationPropsFileName);
   Line := TStringList.Create;
   try
     for I := 0 to Lines.Count - 1 do
     begin
       // Layout of one line is:
       // <range>; <options> [;...] ; # <name>
       SplitLine(Lines[I], Line);
       // continue only if the line is not empty
       if (Line.Count > 0) and (Length(Line[0]) > 1) then
       begin
         // the range currently being under consideration
         SeparatorPos := Pos('..', Line[0]);
         if SeparatorPos > 0 then
         begin
           Start := StrToInt('$' + Copy(Line[0], 1, SeparatorPos - 1));
           Stop := StrToInt('$' + Copy(Line[0], SeparatorPos + 2, MaxInt));
         end
         else
         begin
           Start := StrToInt('$' + Line[0]);
           Stop := Start;
         end;
         // first option is considered
         if SameText(Line[1], 'Full_Composition_Exclusion') then
           AddRangeToCompositionExclusions(Start, Stop);
       end;
       if not Verbose then
         Write(Format(#13'  %d%% done', [Round(100 * I / Lines.Count)]));
     end;
   finally
     Line.Free;
   end;
 finally
   Lines.Free;
 end;
 if not Verbose then
   Writeln;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ParsePropList;

var
 Lines,
 Line: TStringList;
 I, SeparatorPos: Integer;
 Start, Stop: Cardinal;

begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(PropListFileName);
    Line := TStringList.Create;
    try
      for I := 0 to Lines.Count - 1 do
      begin
        // Layout of one line is:
        // <range> or <char>; <property>
        SplitLine(Lines[I], Line);
        // continue only if the line is not empty
        if (Line.Count > 0) and (Length(Line[0]) > 1) then
        begin
          // the range currently being under consideration
          SeparatorPos := Pos('..', Line[0]);
          if SeparatorPos > 0 then
          begin
            Start := StrToInt('$' + Copy(Line[0], 1, SeparatorPos - 1));
            Stop := StrToInt('$' + Copy(Line[0], SeparatorPos + 2, MaxInt));
            AddRangeToCategories(Start, Stop, Line[1]);
          end
          else
          begin
            Start := StrToInt('$' + Line[0]);
            AddToCategories(Start, Line[1]);
          end;
        end;
        if not Verbose then
          Write(Format(#13'  %d%% done', [Round(100 * I / Lines.Count)]));
      end;
    finally
      Line.Free;
    end;
  finally
    Lines.Free;
  end;
  if not Verbose then
    Writeln;
end;

//----------------------------------------------------------------------------------------------------------------------

function FindDecomposition(Code: Cardinal): Integer;

var
  L, R, M: Integer;

begin
  Result := -1;
  L := 0;
  R := High(Decompositions);
  while L <= R do
  begin
    M := (L + R) shr 1;
    if Code > Decompositions[M].Code then
      L := M + 1
    else
      if Code < Decompositions[M].Code then
        R := M - 1
      else
      begin
        Result := M;
        Break;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function DecomposeIt(const S: TDecompositions): TDecompositions;

var
  I, J, K: Integer;
  Sub: TDecompositions;

  procedure AddResult(Code: Cardinal);
  var
    L: Integer;
  begin
    L := Length(Result);
    SetLength(Result, L + 1);
    Result[L] := Code;
  end;

begin
  for I := Low(S) to High(S) do
  begin
    J := FindDecomposition(S[I]);
    if J >= 0 then
    begin
      Sub := DecomposeIt(Decompositions[J].Decompositions);
      for K := Low(Sub) to High(Sub) do
        AddResult(Sub[K]);
    end
    else
      AddResult(S[I]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ExpandDecompositions;

// Expand all decompositions by recursively decomposing each character in the decomposition.

var
  I: Integer;
  S: TDecompositions;
  
begin
  for I := Low(Decompositions) to High(Decompositions) do
  begin
    // avoid side effects by creating a new array
    SetLength(S, 0);
    S := DecomposeIt(Decompositions[I].Decompositions);
    Decompositions[I].Decompositions := S;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function IsCompositionExcluded(Code: Cardinal): Boolean;

// checks if composition is excluded to this code (decomposition cannot be recomposed)

begin
  Result := TestCharacter(CompositionExceptions, Code);
end;

//----------------------------------------------------------------------------------------------------------------------

function SortCompositions(Item1, Item2: Pointer): Integer;
type
  PDecomposition = ^TDecomposition;
var
  Decomposition1, Decomposition2: PDecomposition;
  I, Len1, Len2, MinLen: Integer;
begin
  Decomposition1 := Item1;
  Decomposition2 := Item2;
  Len1 := Length(Decomposition1^.Decompositions);
  Len2 := Length(Decomposition2^.Decompositions);
  MinLen := Len1;
  if MinLen > Len2 then
    MinLen := Len2;

  for I := 0 to MinLen - 1 do
  begin
    if Decomposition1^.Decompositions[I] > Decomposition2^.Decompositions[I] then
    begin
      Result := 1;
      Exit;
    end
    else
    if Decomposition1^.Decompositions[I] < Decomposition2^.Decompositions[I] then
    begin
      Result := -1;
      Exit;
    end;
  end;
  // if starts of two arrays are identical, sorting from longer to shorter (gives more
  // chances to longer combinations at runtime
  if Len1 < Len2 then
    Result := 1
  else
  if Len1 > Len2 then
    Result := -1
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure CreateCompositions;

// create composition list from decomposition list

var
  I, J: Integer;
begin
  // reduce reallocations
  SetLength(Compositions, Length(Decompositions));

  // eliminate exceptions
  I := 0;
  for J := 0 to High(Decompositions) do
    if not IsCompositionExcluded(Decompositions[J].Code) then
  begin
    Compositions[I] := Decompositions[J];
    Inc(I);
  end;

  // fix overhead
  SetLength(Compositions, I);

  SortDynArray(Pointer(Compositions), SizeOf(Compositions[0]), SortCompositions);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure CreateResourceScript;

// creates the target file using the collected data

var
  TextStream, ResourceStream, CompressedResourceStream: TStream;
  CurrentLine: string;

  //--------------- local functions -------------------------------------------

  procedure WriteTextLine(S: AnsiString = '');

  // writes the given string as line into the resource script
  begin
    S := S + #13#10;
    TextStream.WriteBuffer(PAnsiChar(S)^, Length(S));
  end;

  //---------------------------------------------------------------------------

  procedure WriteTextByte(Value: Byte);

  // Buffers one byte of data (conversion to two-digit hex string is performed first)
  // and flushs out the current line if there are 32 values collected.

  begin
    CurrentLine := CurrentLine + Format('%.2x ', [Value]);
    if Length(CurrentLine) = 32 * 3 then
    begin
      WriteTextLine(AnsiString('  ''' + Trim(CurrentLine) + ''''));
      CurrentLine := '';
    end;
  end;

  //---------------------------------------------------------------------------

  procedure WriteResourceByte(Value: Cardinal);

  begin
    if Value <= $FF then
      ResourceStream.WriteBuffer(Value, 1)
    else
      FatalError('byte out of bound');
  end;

  //---------------------------------------------------------------------------

  procedure WriteResourceCardinal(Value: Cardinal);

  begin
    ResourceStream.WriteBuffer(Value, SizeOf(Value));
  end;

  //---------------------------------------------------------------------------

  procedure WriteResourceChar(Value: Cardinal);

  begin
    if Value < $1000000 then
      ResourceStream.WriteBuffer(Value, 3)
    else
      FatalError('character out of bound');
  end;

  //---------------------------------------------------------------------------

  procedure WriteResourceCharArray(Values: array of Cardinal);

  // loops through Values and writes them into the target file

  var
    I: Integer;

  begin
    for I := Low(Values) to High(Values) do
      WriteResourceChar(Values[I]);
  end;

  //---------------------------------------------------------------------------

  procedure CreateResource;

  begin
    if ZLibCompress or BZipCompress then
      CompressedResourceStream := TMemoryStream.Create;
    if ZLibCompress then
      ResourceStream := TJclZLibCompressStream.Create(CompressedResourceStream, 9)
    else
    if BZipCompress then
      ResourceStream := TJclBZIP2CompressionStream.Create(CompressedResourceStream, 9)
    else
      ResourceStream := TMemoryStream.Create;
  end;

  //---------------------------------------------------------------------------

  procedure FlushResource;

  var
    Buffer: Byte;

  begin
    if ZLibCompress or BZipCompress then
    begin
      ResourceStream.Free;

      ResourceStream := CompressedResourceStream;
    end;

    ResourceStream.Seek(0, soFromBeginning);

    while ResourceStream.Read(Buffer, SizeOf(Buffer)) = SizeOf(Buffer) do
      WriteTextByte(Buffer);

    ResourceStream.Free;

    if Length(CurrentLine) > 0 then
    begin
      WriteTextLine(AnsiString('  ''' + Trim(CurrentLine) + ''''));
      CurrentLine := '';
    end;
  end;

  //--------------- end local functions ---------------------------------------

var
  I, J: Integer;
  Ranges: TRangeArray;
  Category: TCharacterCategory;

begin
  CurrentLine := '';
  TextStream := TFileStream.Create(TargetFileName, fmCreate);
  try
    // 1) template header
    WriteTextLine(AnsiString('/' + StringOfChar('*', 100)));
    WriteTextLine;
    WriteTextLine;
    WriteTextLine(AnsiString('  ' + TargetFileName));
    WriteTextLine;
    WriteTextLine;
    WriteTextLine('  Produced by UDExtract written by Dipl. Ing. Mike Lischke, public@lischke-online.de');
    WriteTextLine;
    WriteTextLine;
    WriteTextLine(AnsiString(StringOfChar('*', 100) + '/'));
    WriteTextLine;
    WriteTextLine;

    // 2) category data
    WriteTextLine('LANGUAGE 0,0 CATEGORIES UNICODEDATA LOADONCALL MOVEABLE DISCARDABLE');
    WriteTextLine('{');
    CreateResource;
    // write out only used categories
    for Category := Low(TCharacterCategory) to High(TCharacterCategory) do
    begin
      Ranges := FindCharacterRanges(Categories[Category]);
      if Length(Ranges) > 0 then
      begin
        // a) record what category it is actually (the cast assumes there will never
        //    be more than 256 categories)
        WriteResourceByte(Ord(Category));
        // b) tell how many ranges are assigned
        WriteResourceCardinal(Length(Ranges));
        // c) write start and stop code of each range
        for J := Low(Ranges) to High(Ranges) do
        begin
          WriteResourceChar(Ranges[J].Start);
          WriteResourceChar(Ranges[J].Stop);
        end;
      end;
    end;

    FlushResource;
    WriteTextLine('}');
    WriteTextLine;
    WriteTextLine;

    // 3) case mapping data
    WriteTextLine('LANGUAGE 0,0 CASE UNICODEDATA LOADONCALL MOVEABLE DISCARDABLE');
    WriteTextLine('{');
    CreateResource;
    // record how many case mapping entries we have
    WriteResourceCardinal(Length(CaseMapping));
    for I := 0 to High(CaseMapping) do
    begin
      // store every available case mapping, consider one-to-many mappings
      // a) write actual code point
      WriteResourceChar(CaseMapping[I].Code);
      // b) write lower case
      WriteResourceByte(Length(CaseMapping[I].Fold));
      WriteResourceCharArray(CaseMapping[I].Fold);
      // c) write lower case
      WriteResourceByte(Length(CaseMapping[I].Lower));
      WriteResourceCharArray(CaseMapping[I].Lower);
      // d) write title case
      WriteResourceByte(Length(CaseMapping[I].Title));
      WriteResourceCharArray(CaseMapping[I].Title);
      // e) write upper case
      WriteResourceByte(Length(CaseMapping[I].Upper));
      WriteResourceCharArray(CaseMapping[I].Upper);
    end;
    FlushResource;
    WriteTextLine('}');
    WriteTextLine;
    WriteTextLine;

    // 4) decomposition data
    // fully expand all decompositions before generating the output
    ExpandDecompositions;
    WriteTextLine('LANGUAGE 0,0 DECOMPOSITION UNICODEDATA LOADONCALL MOVEABLE DISCARDABLE');
    WriteTextLine('{');
    CreateResource;
    // record how many decomposition entries we have
    WriteResourceCardinal(Length(Decompositions));
    for I := 0 to High(Decompositions) do
    begin
      WriteResourceChar(Decompositions[I].Code);
      WriteResourceByte(Length(Decompositions[I].Decompositions));
      WriteResourceByte(Byte(Decompositions[I].Tag));
      WriteResourceCharArray(Decompositions[I].Decompositions);
    end;
    FlushResource;
    WriteTextLine('}');
    WriteTextLine;
    WriteTextLine;

    // 5) canonical combining class data
    WriteTextLine('LANGUAGE 0,0 COMBINING UNICODEDATA LOADONCALL MOVEABLE DISCARDABLE');
    WriteTextLine('{');
    CreateResource;
    for I := 0 to 255 do
    begin
      Ranges := FindCharacterRanges(CCCs[I]);
      if Length(Ranges) > 0 then
      begin
        // a) record which class is stored here
        WriteResourceByte(I);
        // b) tell how many ranges are assigned
        WriteResourceByte(Length(Ranges));
        // c) write start and stop code of each range
        for J := Low(Ranges) to High(Ranges) do
        begin
          WriteResourceChar(Ranges[J].Start);
          WriteResourceChar(Ranges[J].Stop);
        end;
      end;
    end;

    FlushResource;
    WriteTextLine('}');
    WriteTextLine;
    WriteTextLine;

    // 6) number data, this is actually two arrays, one which contains the numbers
    //    and the second containing the mapping between a code and a number
    WriteTextLine('LANGUAGE 0,0 NUMBERS UNICODEDATA LOADONCALL MOVEABLE DISCARDABLE');
    WriteTextLine('{');
    CreateResource;
    // first, write the number definitions (size, values)
    WriteResourceByte(Length(Numbers));
    for I := 0 to High(Numbers) do
    begin
      WriteResourceCardinal(Cardinal(Numbers[I].Numerator));
      WriteResourceCardinal(Cardinal(Numbers[I].Denominator));
    end;
    // second, write the number mappings (size, values)
    WriteResourceCardinal(Length(NumberCodes));
    for I := 0 to High(NumberCodes) do
    begin
      WriteResourceChar(NumberCodes[I].Code);
      WriteResourceByte(NumberCodes[I].Index);
    end;
    FlushResource;
    WriteTextLine('}');
    WriteTextLine;
    WriteTextLine;

    // 7 ) composition data
    // create composition data from decomposition data and exclusion list before generating the output
    CreateCompositions;
    WriteTextLine('LANGUAGE 0,0 COMPOSITION UNICODEDATA LOADONCALL MOVEABLE DISCARDABLE');
    WriteTextLine('{');
    CreateResource;
    // first, write the number of compositions
    WriteResourceCardinal(Length(Compositions));
    for I := 0 to High(Compositions) do
    begin
      WriteResourceChar(Compositions[I].Code);
      WriteResourceByte(Length(Compositions[I].Decompositions));
      WriteResourceByte(Byte(Compositions[I].Tag));
      WriteResourceCharArray(Compositions[I].Decompositions);
    end;
    FlushResource;
    WriteTextLine('}');
  finally
    TextStream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
procedure PrintUsage;

begin
  Writeln('Usage: UDExtract Source[.txt] Target[.rc] options');
  Writeln('  Path and extension are optional. Default extension for all source files');
  Writeln('  (including optional files) is ".txt".');
  Writeln('  Source must be a Unicode data file (e.g. UnicodeData-5.0.0.txt)');
  Writeln('  and Target is a resource script.');
  Writeln;
  Writeln('  Options might have the following values (not case sensitive):');
  Writeln('    /?'#9#9'shows this screen');
  Writeln('    /c=filename'#9'specifies an optional file containing special casing');
  Writeln('    '#9#9'properties (e.g. SpecialCasing-5.0.0.txt)');
  Writeln('    /f=filename'#9'specifies an optional file containing case fold');
  Writeln('    '#9#9'mappings (e.g. CaseFolding-5.0.0.txt)');
  WriteLn('    /d=filename'#9'specifies an optional file containing derived normalization');
  WriteLn('    '#9#9'props (e.g. DerivedNormalizationProps-5.0.0.txt)');
  WriteLn('    /p=filename'#9'specifies an optional file containing the list of');
  WriteLn('    '#9#9'character properties (e.g. PropList-5.0.0.txt)');
  Writeln('    /v'#9#9'verbose mode; no warnings, errors etc. are shown, no user input is required');
  WriteLn('    /z'#9#9'compress resource streams using zlib');
  WriteLn('    /bz'#9#9'compress resource streams using bzip2');
  Writeln;
  Writeln('Press <enter> to continue...');
  Readln;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure CheckExtension(var FileName: TFileName; const Ext: String);

// Checks whether the given file name string contains an extension. If not then Ext is added to FileName.

begin
  if ExtractFileExt(FileName) = '' then
    FileName := FileName + Ext;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ParseOptions;

var
  I: Integer;
  S: string;
  Run: PChar;

begin
  for I := 3 to ParamCount do
  begin
    S := Trim(ParamStr(I));
    if (Length(S) = 0) or (S[1] <> '/') then
    begin
      Halt(2);
    end
    else
    if SameText(S, '/v') then
      Verbose := True
    else
    if SameText(S, '/z') then
      ZLibCompress := True
    else
    if SameText(S, '/bz') then
      BZipCompress := True
    else
    if SameText(Copy(S, 1, 3), '/c=') then
    begin
      SpecialCasingFileName := Trim(Copy(S, 4, MaxInt));
      if (SpecialCasingFileName[1] = '''') or (SpecialCasingFileName[1] = '"') then
      begin
        Run := PChar(SpecialCasingFileName);
        SpecialCasingFileName := Trim(AnsiExtractQuotedStr(Run, SpecialCasingFileName[1]));
      end;
      CheckExtension(SpecialCasingFileName, '.txt');
    end
    else
    if SameText(Copy(S, 1, 3), '/f=') then
    begin
      CaseFoldingFileName := Trim(Copy(S, 4, MaxInt));
      if (CaseFoldingFileName[1] = '''') and (CaseFoldingFileName = '"') then
      begin
        Run := PChar(CaseFoldingFileName);
        CaseFoldingFileName := Trim(AnsiExtractQuotedStr(Run, CaseFoldingFileName[1]));
      end;
      CheckExtension(CaseFoldingFileName, '.txt');
    end
    else
    if SameText(Copy(S, 1, 3), '/d=') then
    begin
      DerivedNormalizationPropsFileName := Trim(Copy(S, 4, MaxInt));
      if (DerivedNormalizationPropsFileName[1] = '''') or (DerivedNormalizationPropsFileName[1] = '"') then
      begin
        Run := PChar(DerivedNormalizationPropsFileName);
        DerivedNormalizationPropsFileName := Trim(AnsiExtractQuotedStr(Run, DerivedNormalizationPropsFileName[1]));
      end;
      CheckExtension(DerivedNormalizationPropsFileName, '.txt');
    end
    else
    if SameText(Copy(S, 1, 3), '/p=') then
    begin
      PropListFileName := Trim(Copy(S, 4, MaxInt));
      if (PropListFileName[1] = '''') or (PropListFileName[1] = '"') then
      begin
        Run := PChar(PropListFileName);
        PropListFileName := Trim(AnsiExtractQuotedStr(Run, PropListFileName[1]));
      end;
      CheckExtension(PropListFileName, '.txt');
    end
    else
    begin
      PrintUsage;
      if SameText(S, '/?') then
        Halt(0)
      else
        Halt(2);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

begin
  Writeln('Unicode database conversion tool');
  Writeln(#$B8' 2000, written by Dipl. Ing. Mike Lischke [public@lischke-online.de]');
  Writeln;

  if ParamCount = 0 then
    PrintUsage
  else
  try
    ParseOptions;

    if BZipCompress and not LoadBZip2 then
    begin
      WriteLn('failed to load bzip2 library');
      Halt(1);
    end;

    SourceFileName := Trim(ParamStr(1));
    CheckExtension(SourceFileName, '.txt');
    TargetFileName := Trim(ParamStr(2));
    CheckExtension(TargetFileName, '.rc');

    if not FileExists(SourceFileName) then
    begin
      if not Verbose then
        Writeln(Format('[Fatal error] ''%s'' not found', [SourceFileName]));
      Halt(1);
    end
    else
    begin
      if not Verbose then
      begin
        Writeln;
        Writeln('Reading data from ' + SourceFileName + ':');
      end;
      ParseData;

      // optional parsing parts
      if Length(SpecialCasingFileName) > 0 then
      begin
        if not FileExists(SpecialCasingFileName) then
        begin
          Writeln;
          Warning(SpecialCasingFileName + ' not found, ignoring special casing');
        end
        else
        begin
          if not Verbose then
          begin
            Writeln;
            Writeln('Reading special casing data from ' + SpecialCasingFileName + ':');
          end;
          ParseSpecialCasing;
        end;
      end;

      if Length(CaseFoldingFileName) > 0 then
      begin
        if not FileExists(CaseFoldingFileName) then
        begin
          Writeln;
          Warning(CaseFoldingFileName + ' not found, ignoring case folding');
        end
        else
        begin
          if not Verbose then
          begin
            Writeln;
            Writeln('Reading case folding data from ' + CaseFoldingFileName + ':');
          end;
          ParseCaseFolding;
        end;
      end;

      if Length(DerivedNormalizationPropsFileName) > 0 then
      begin
        if not FileExists(DerivedNormalizationPropsFileName) then
        begin
          WriteLn;
          Warning(DerivedNormalizationPropsFileName + ' not found, ignoring derived normalization');
        end
        else
        begin
          if not Verbose then
          begin
            WriteLn;
            WriteLn('Reading derived normalization props from ' + DerivedNormalizationPropsFileName + ':');
          end;
          ParseDerivedNormalizationProps;
        end;
      end;

      if Length(PropListFileName) > 0 then
      begin
        if not FileExists(PropListFileName) then
        begin
          WriteLn;
          Warning(PropListFileName + ' not found, ignoring property list');
        end
        else
        begin
          if not Verbose then
          begin
            WriteLn;
            WriteLn('Reading property list from ' + PropListFileName + ':');
          end;
          ParsePropList;
        end;
      end;

      // finally write the collected data
      if not Verbose then
      begin
        Writeln;
        Writeln;
        Writeln('Writing resource script ' + TargetFileName + '  ');
        CreateResourceScript;
      end;
    end;

  finally
    if not Verbose then
    begin
      Writeln;
      Writeln('Program finished. Press <enter> to continue...');
      ReadLn;
    end;
  end;
end.

