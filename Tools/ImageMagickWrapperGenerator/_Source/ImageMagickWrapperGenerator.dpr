program ImageMagickWrapperGenerator;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  system.AnsiStrings,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.SysUtils,
  System.IOUtils,
  Alcinoe.files,
  Alcinoe.StringList,
  Alcinoe.StringUtils,
  Alcinoe.common;

{****************************************************************************************}
function RemoveSection(const ACSource: AnsiString; const ADefine: AnsiString): AnsiString;
begin

  var LDefine := ADefine;
  var LInverse := ALposA('!', LDefine) = 1;
  if LInverse then delete(LDefine, 1, 1);
  var LIgnoreLine := False;

  var LIn := TALStringListA.Create;
  var LOut := TALStringListA.Create;
  try
    LIn.Text := ACSource;
    var LSkipDepth: Integer := 0;

    for var i := 0 to LIn.Count - 1 do begin
      var LLine := LIn[i];
      var LTrim := ALTrim(LLine);

      // Start of a block to skip?
      // e.g. "#if defined(MAGICKCORE_X11_DELEGATE)"
      if (LSkipDepth = 0) and
         (ALPosIgnoreCaseA(LDefine, LTrim) > 0) then begin
        if ALPosIgnoreCaseA(LTrim, '#if !' + LDefine) = 1 then begin
          if (Not ALSameTextA(LTrim, '#if !' + LDefine)) then
            Raise Exception.Create('Error BF1D0AD8-3B8F-4EEA-8253-311FB0A61682');
          Inc(LSkipDepth);
          LOut.Add(LLine); // keep the #if line itself
          LIgnoreLine := LInverse;
          Continue;
        end
        else begin
          if (Not ALSameTextA(LTrim, '#if ' + LDefine)) and
             (Not ALSameTextA(LTrim, '#if defined(MAGICKCORE_CIPHER_SUPPORT) && defined(_MSC_VER)')) then
            Raise Exception.Create('Error B9630920-CB07-49CC-BAEC-2ABB105D2F9C');
          Inc(LSkipDepth);
          LOut.Add(LLine); // keep the #if line itself
          LIgnoreLine := not LInverse;
          Continue;
        end;
      end;

      // We are currently skipping a block that started with "#if ... LDefine ..."
      if LSkipDepth > 0 then begin
        // If we hit an #else or #elif at the *top* of this skipped block,
        // we stop skipping and keep everything from here on.
        if (LSkipDepth = 1) and
           ( (ALPosIgnoreCaseA('#else', LTrim) = 1) or
             (ALPosIgnoreCaseA('#elif', LTrim) = 1) ) then begin
          LOut.Add(LLine);      // keep the #else / #elif line itself
          LIgnoreLine := not LIgnoreLine;
          Continue;
        end;

        // Nested #if inside the skipped block
        if (ALPosIgnoreCaseA('#if', LTrim) = 1) then begin
          Inc(LSkipDepth);
          If not LIgnoreLine then LOut.Add(LLine);
          Continue;
        end;

        // Matching #endif (possibly for nested levels)
        if (ALPosIgnoreCaseA('#endif', LTrim) = 1) then begin
          Dec(LSkipDepth);
          If (LSkipDepth = 0) or (not LIgnoreLine) then LOut.Add(LLine);
          Continue;
        end;

        // Any normal line inside the skipped region
        If not LIgnoreLine then LOut.Add(LLine);
        Continue;
      end;

      // Not in a skipped block ⇒ keep the line
      LOut.Add(LLine);
    end;

    Result := LOut.Text;
  finally
    ALFreeAndNil(LIn);
    ALFreeAndNil(LOut);
  end;

end;

{**********************************************************}
function CType2Delphi(const ACtype: AnsiString): AnsiString;
begin
  if ALSameTextA(ACtype, '_ImageInfo') then Result := 'ImageInfo'
  else if ALSameTextA(ACtype, 'float') then Result := 'Single'
  else if ALSameTextA(ACtype, 'char') then Result := 'AnsiChar'
  else if ALSameTextA(ACtype, 'unsigned') then Result := 'Cardinal'
  else if ALSameTextA(ACtype, 'unsigned char') then Result := 'Byte'
  else if ALSameTextA(ACtype, 'int') then Result := 'Integer'
  else if ALSameTextA(ACtype, 'signed int') then Result := 'Integer'
  else if ALSameTextA(ACtype, 'signed short') then Result := 'SmallInt'
  else if ALSameTextA(ACtype, 'unsigned short') then Result := 'Word'
  else if ALSameTextA(ACtype, 'unsigned short int') then Result := 'Word'
  else if ALSameTextA(ACtype, 'unsigned int') then Result := 'Cardinal'
  else if ALSameTextA(ACtype, 'unsigned long') then Result := 'LongWord'
  else if ALSameTextA(ACtype, 'long double') then Result := 'Extended'
  else if ALSameTextA(ACtype, 'unsigned_char') then Result := 'Byte'
  else if ALSameTextA(ACtype, 'signed_int') then Result := 'Integer'
  else if ALSameTextA(ACtype, 'signed_short') then Result := 'SmallInt'
  else if ALSameTextA(ACtype, 'unsigned_short') then Result := 'Word'
  else if ALSameTextA(ACtype, 'unsigned_short_int') then Result := 'Word'
  else if ALSameTextA(ACtype, 'unsigned_int') then Result := 'Cardinal'
  else if ALSameTextA(ACtype, 'unsigned_long') then Result := 'LongWord'
  else if ALSameTextA(ACtype, 'long_double') then Result := 'Extended'
  else Result := ACtype;
end;

{***************************************************************}
function CVarName2Delphi(const AVarName: AnsiString): AnsiString;
begin
  if ALSameTextA(AVarName, 'type') then Result := '&type'
  else if ALSameTextA(AVarName, 'file') then Result := '&file'
  else if ALSameTextA(AVarName, 'function') then Result := '&function'
  else if ALSameTextA(AVarName, 'type') then Result := '&type'
  else if ALSameTextA(AVarName, 'label') then Result := '&label'
  else if ALSameTextA(AVarName, 'file') then Result := '&file'
  else if ALSameTextA(AVarName, 'raise') then Result := '&raise'
  else if ALSameTextA(AVarName, 'string') then Result := '&string'
  else if ALSameTextA(AVarName, 'set') then Result := '&set'
  else if ALSameTextA(AVarName, 'array') then Result := '&array'
  else if ALSameTextA(AVarName, 'property') then Result := '&property'
  else if ALSameTextA(AVarName, 'end') then Result := '&end'
  else if ALSameTextA(AVarName, 'destructor') then Result := '&destructor'
  else Result := AVarName;
end;

{*****************************************************************************}
procedure ExtractEnums(Const ACFilename: String; var ADelphiEnums: AnsiString);
begin

  //
  // typedef enum
  // {
  //   UndefinedEndian,
  //   LSBEndian,
  //   MSBEndian
  // } EndianType;
  //

  Var LAddTypeKeyword: Boolean := False;
  var LCSrc := ALGetStringFromFile(ACFilename);
  LCSrc := ALStringReplaceA(LCSrc, #9, ' ');
  LCSrc := ALStringReplaceA(LCSrc, #13, '');
  LCSrc := ALStringReplaceA(LCSrc, #10, #13#10);
  var P1 := ALPosIgnoreCaseA('typedef enum', LCSrc);
  While P1 > 0 do begin
    P1 := AlposA('{', LCSrc, P1);
    if P1 <= 0 then
      Raise Exception.Create('Error 2BB82E08-61E1-4EED-AD14-38922854B20B');
    inc(P1);
    var P2 := AlposA('}', LCSrc, P1);
    if P2 <= 0 then
      raise Exception.Create('Error 0E8E2A21-9592-41E5-9F0B-893FF4F265A9');
    var P3 := AlposA(';', LCSrc, P2);
    if P3 <= 0 then
      raise Exception.Create('Error C28FFD5A-735F-45E0-82B7-BC4489989606');
    var LValuesStr := ALTrim(AlCopyStr(LCSrc, P1, P2 - P1));
    If LValuesStr = '' then
      Raise Exception.Create('Error 0AE93F26-3A90-49B3-B1FA-CDA0BA31085B');
    LValuesStr := ALStringReplaceA(LValuesStr, '/*', '{');
    LValuesStr := ALStringReplaceA(LValuesStr, '*/', '}');
    LValuesStr := ALStringReplaceA(LValuesStr, '0x', '$');
    var LNameStr := ALTrim(AlCopyStr(LCSrc, P2+1, P3 - P2-1));
    If LNameStr = '' then
      Raise Exception.Create('Error EC25D55C-DC20-4F8F-9A11-BC5E522AB8CB');
    var LLst := TALStringListA.Create;
    try
      LLst.Text := LValuesStr;
      for var i := LLst.Count - 1 downto 0 do
        if ALPosA('#undef ', ALTrim(LLst[I])) = 1 then
          LLst.Delete(I);
      for var I := 0 to LLst.Count - 1 do begin
        var LLine := LLst[I];
        if (I = LLst.Count - 1) then begin
          var P4 := ALPosA('{', LLine);
          IF (P4 > 0) then begin
            dec(P4);
            While (P4 > low(LLine)) and (LLine[P4] = ' ') do dec(P4);
            if P4 = low(LLine) then Raise Exception.Create('Error F0C675E4-E4E1-4FD8-BF35-3AA96476DD3E');
            Inc(P4);
            Insert(');', LLine, P4);
            LLst[I] := '    ' + ALTrim(LLine);
          end
          else
            LLst[I] := '    ' + ALTrim(LLine) + ');';
        end
        else
          LLst[I] := ALTrimRight('    ' + ALTrim(LLine));
      end;
      LValuesStr := ALTrimRight(LLst.Text);
    finally
      ALFreeAndNil(LLst);
    end;

    LValuesStr := ALStringReplaceA(LValuesStr, 'UndefinedColorInterpolate = UndefinedDistortion', 'UndefinedColorInterpolate = Ord(UndefinedDistortion)');
    LValuesStr := ALStringReplaceA(LValuesStr, 'BarycentricColorInterpolate = AffineDistortion', 'BarycentricColorInterpolate = Ord(AffineDistortion)');
    LValuesStr := ALStringReplaceA(LValuesStr, 'BilinearColorInterpolate = BilinearReverseDistortion', 'BilinearColorInterpolate = Ord(BilinearReverseDistortion)');
    LValuesStr := ALStringReplaceA(LValuesStr, 'PolynomialColorInterpolate = PolynomialDistortion', 'PolynomialColorInterpolate = Ord(PolynomialDistortion)');
    LValuesStr := ALStringReplaceA(LValuesStr, 'ShepardsColorInterpolate = ShepardsDistortion', 'ShepardsColorInterpolate = Ord(ShepardsDistortion)');
    LValuesStr := ALStringReplaceA(LValuesStr, 'VoronoiColorInterpolate = SentinelDistortion', 'VoronoiColorInterpolate = Ord(SentinelDistortion)');

    if ALSameTextA(LNameStr, 'ChannelType') then begin
      LValuesStr := ALStringReplaceA(LValuesStr, ',', ';');
      LValuesStr := ALStringReplaceA(LValuesStr, ');', ';');
      LValuesStr := ALStringReplaceA(LValuesStr, '#if defined(MAGICKCORE_64BIT_CHANNEL_MASK_SUPPORT)', '{$IF defined(MAGICKCORE_64BIT_CHANNEL_MASK_SUPPORT)}');
      LValuesStr := ALStringReplaceA(LValuesStr, '#else', '{$ELSE}');
      LValuesStr := ALStringReplaceA(LValuesStr, '#endif', '{$ENDIF}');
      LValuesStr := ALStringReplaceA(LValuesStr, ' = ', ': ChannelType = ');
      LValuesStr := ALStringReplaceA(LValuesStr, 'MagickLLConstant($7FFFFFFFFFFFFFFF;', '$7FFFFFFFFFFFFFFF;');
      LValuesStr := ALStringReplaceA(LValuesStr, #13#10'    ', #13#10'  ');

      LValuesStr := ALStringReplaceA(
                      LValuesStr,
                      'DefaultChannels: ChannelType = AllChannels;',
                      '{$IF defined(MAGICKCORE_64BIT_CHANNEL_MASK_SUPPORT)}' + #13#10 +
                      '  DefaultChannels: ChannelType = $7FFFFFFFFFFFFFFF; {AllChannels}' + #13#10 +
                      '  {$ELSE}' + #13#10 +
                      '  DefaultChannels: ChannelType = $7FFFFFF; {AllChannels}' + #13#10 +
                      '  {$ENDIF}');

      //
      // #if defined(MAGICKCORE_64BIT_CHANNEL_MASK_SUPPORT)
      // typedef enum : MagickOffsetType
      // #else
      // typedef enum
      // #endif
      //

      ADelphiEnums := ADelphiEnums + ALIfThenA(ADelphiEnums <> '', #13#10#13#10) +
                      '  {$IF defined(MAGICKCORE_64BIT_CHANNEL_MASK_SUPPORT)}' + #13#10 +
                      '  '+LNameStr + ' = MagickOffsetType;' + #13#10 +
                      '  {$ELSE}' + #13#10 +
                      '  '+LNameStr + ' = Integer;' + #13#10 +
                      '  {$ENDIF}'+ #13#10 +
                      #13#10 +
                      'const'+ #13#10 +
                      '  ' + ALTrim(LValuesStr);

      LAddTypeKeyword := True;

    end
    else begin
      ADelphiEnums := ADelphiEnums + ALIfThenA(ADelphiEnums <> '', #13#10#13#10) +
                      ALIfThenA(LAddTypeKeyword, 'Type' + #13#10) +
                      '  ' + LNameStr + ' = (' + #13#10 +
                      LValuesStr + #13#10 +
                      '  P'+LNameStr + ' = ^'+LNameStr+';';
      LAddTypeKeyword := False;
    end;
    P1 := ALPosIgnoreCaseA('typedef enum', LCSrc, P3);
  end;

end;

{*********************************************************************************}
procedure ExtractRecords(Const ACFilename: String; var ADelphiRecords: AnsiString);
begin

  //
  // typedef struct _ElementReference
  // {
  //   char
  //     *id;
  //
  //   ReferenceType
  //     type;
  //
  //   GradientInfo
  //     gradient;
  //
  //   struct _ElementReference
  //     *previous,
  //     *next;
  //
  //   size_t
  //     signature;
  // } ElementReference;
  //

  //
  // struct ReferenceFormats
  // {
  //   const char
  //     *magick;
  //
  //   CompressionType
  //     compression;
  //
  //   double
  //     fuzz;
  // };
  //

  //
  // typedef struct
  // {
  //   FILE
  //     *stream;        /* the file stream we are reading from */
  //
  //   MagickBooleanType
  //     opened;         /* was that stream opened? */
  //
  //   char
  //     *token;         /* array of characters to holding details of he token */
  //
  //   size_t
  //     length,         /* length of token char array */
  //     curr_line,      /* current location in script file */
  //     curr_column,
  //     token_line,      /* start of last token (option or argument) */
  //     token_column;
  //
  //   TokenStatus
  //     status;         /* Have we reached EOF? see Token Status */
  //
  //   size_t
  //     signature;
  // } ScriptTokenInfo;
  //

  var LCSrc := ALGetStringFromFile(ACFilename);
  LCSrc := ALStringReplaceA(LCSrc, #9, ' ');
  LCSrc := ALStringReplaceA(LCSrc, #13, '');
  LCSrc := ALStringReplaceA(LCSrc, #10, #13#10);
  LCSrc := ALStringReplaceA(LCSrc, #13#10'struct ', ' struct ');
  LCSrc := ALStringReplaceA(LCSrc, ' struct'#13#10, ' struct ');
  LCSrc := ALStringReplaceA(LCSrc, 'typedef struct _Image Image;', ''); // Forward declaration
  LCSrc := ALStringReplaceA(LCSrc, 'typedef struct _ImageInfo ImageInfo;', ''); // Forward declaration
  LCSrc := ALStringReplaceA(LCSrc, 'typedef struct _ExceptionInfo ExceptionInfo;', ''); // Forward declaration

  While ALPosA(' * ',LCSrc) > 0 do
    LCSrc := ALStringReplaceA(LCSrc, ' * ',' *');

  var P1 := ALPosIgnoreCaseA(' struct ', LCSrc);
  While P1 > 0 do begin
    inc(P1, length(' struct '));
    var P2 := AlposA('{', LCSrc, P1);
    var P3 := AlposA(';', LCSrc, P1);
    if P3 <= 0 then
      Raise Exception.Create('Error C012FCA2-4DB8-4992-AA9B-6F21F91556DC');
    if (P2 <= 0) or (P2 >= P3) then begin
      //
      // typedef struct _CustomStreamInfo
      //   CustomStreamInfo;
      //
      P2 := P1;
      While (P2 <= high(LCSrc)) and ((LCSrc[P2]) not in [' ', #13]) do inc(P2);
      if P2 > high(LCSrc) then
        raise Exception.Create('Error 1534F370-7063-4639-8271-B7328A9F7B00');
      insert(' {'#13#10'}', LCSrc, P2);
      inc(P2);
    end;
    inc(P2);
    P3 := AlposA('}', LCSrc, P2);
    if P3 <= 0 then
      raise Exception.Create('Error 912AD4CB-F878-46F0-9C80-C5728F087F0A');
    var P4 := AlposA(';', LCSrc, P3);
    if P4 <= 0 then
      raise Exception.Create('Error 6D91974E-4131-44D4-B2E2-1749BC771882');
    var LValuesStr := ALTrim(AlCopyStr(LCSrc, P2, P3 - P2));

    var P5 := ALPosA('/*', LValuesStr);
    while P5 > 0 do begin
      var P6 := ALPosA('*/', LValuesStr, P5);
      if P6 <= 0 then
        raise Exception.Create('Error D835B063-D520-43F9-9868-8B96AFF04E8D');
      inc(P6,2);
      var LComment := ALCopyStr(LValuesStr, P5, P6-P5);
      while AlposA(#13#10' ', LComment) > 0 do
        LComment := ALStringReplaceA(LComment, #13#10' ', #13#10);
      LComment := ALStringReplaceA(LComment, #13#10, ' ');
      LComment := ALStringReplaceA(LComment, ' * ', ' ');
      delete(LValuesStr, P5, P6-P5);
      insert(LComment, LValuesStr, P5);
      P5 := ALPosA('/*', LValuesStr, P5+1);
    end;
    LValuesStr := ALStringReplaceA(LValuesStr, '/*', '{');
    LValuesStr := ALStringReplaceA(LValuesStr, '*/', '}');

    var LName1Str := ALTrim(AlCopyStr(LCSrc, P1, P2 - P1 - 1));
    var LName2Str := ALTrim(AlCopyStr(LCSrc, P3+1, P4 - P3 - 1));
    var LPackedRecord: Boolean := False;
    var LLst := TALStringListA.Create;
    try
      LLst.Text := ALTrim(LValuesStr);
      var LCurrentType: AnsiString := '';
      var LCurrentTypeUsed: Boolean := True;
      var I: Integer := 0;
      While I <= LLst.Count - 1 do begin
        var LLine := ALTrim(LLst[I]);
        if LLine = '' then begin
          LLst.Delete(I);
          Continue;
        end;
        if (LLine[low(LLine)] = '{') and (LLine[high(LLine)] = '}') then begin
          LLst[I] := '    ' + LLine;
          inc(i);
          Continue;
        end;
        If (ALPosA(',', LLine) <= 0) and (ALPosA(';', LLine) <= 0) then begin
          if not LCurrentTypeUsed then
            Raise Exception.Create('Error 75379BD3-1932-4FDF-8267-A58ADA89BC63');
          LCurrentTypeUsed := False;
          LCurrentType := LLine;
          if ALSameTextA(LCurrentType, '#pragma __nomember_alignment') then begin
            LCurrentType := '';
            LCurrentTypeUsed := True;
            LPackedRecord := True;
            LLst.Delete(I);
            Continue;
          end;
          if AlposIgnoreCaseA('const ', LCurrentType) = 1 then begin
            delete(LCurrentType, 1, length('const '));
            LCurrentType := ALTrim(LCurrentType);
          end;
          if AlposIgnoreCaseA('struct ', LCurrentType) = 1 then begin
            delete(LCurrentType, 1, length('struct '));
            LCurrentType := ALTrim(LCurrentType);
          end;
          LCurrentType := CType2Delphi(LCurrentType);
          LLst.Delete(I);
          Continue;
        end;
        var LFieldName := LLine;
        var LComment: AnsiString := '';
        P5 := ALPosA('{', LFieldName);
        if P5 > 0 then begin
          var P6 := ALPosA('}', LFieldName);
          if P6 <= 0 then
            Raise exception.Create('Error EEF64FC8-6BF6-42C2-8DBC-4AEB41E8E0CD');
          LComment := ' ' + AlcopyStr(LFieldName, P5, P6-P5+1);
          delete(LFieldName, P5, P6-P5+1);
          LFieldName := ALTrim(LFieldName);
        end;
        LFieldName := ALStringReplaceA(LFieldName, ',', '');
        LFieldName := ALStringReplaceA(LFieldName, ';', '');
        LFieldName := AlTrim(LFieldName);
        if ALSameTextA(LFieldName, '*decoder') and ALSameTextA(LCurrentType, 'DecodeImageHandler') then LFieldName := 'decoder'
        else if ALSameTextA(LFieldName, '*encoder') and ALSameTextA(LCurrentType, 'EncodeImageHandler') then LFieldName := 'encoder'
        else if ALSameTextA(LFieldName, '*magick') and ALSameTextA(LCurrentType, 'IsImageFormatHandler') then LFieldName := 'magick';
        var LPointerPrefix: ansiString := '';
        while AlposA('*', LFieldName) = 1 do begin
          LPointerPrefix := LPointerPrefix + 'P';
          delete(LFieldName, low(LFieldName), 1);
        end;
        var LArrayPrefix: AnsiString := '';
        P5 := ALposA('[', LFieldName); // invariant[MaximumNumberOfImageMoments+1]
        if P5 > 0 then begin
          LArrayPrefix := AlcopyStr(LFieldName, P5, MaxInt);  // [MaximumNumberOfImageMoments+1]
          LFieldName := ALTrim(AlcopyStr(LFieldName, 1, P5-1));  // invariant
          LArrayPrefix := ALStringReplaceA(LArrayPrefix, '[', 'array[0..');
          LArrayPrefix := ALStringReplaceA(LArrayPrefix, ']', '-1]');
          LArrayPrefix := ALStringReplaceA(LArrayPrefix, '+1-1]', ']');
          LArrayPrefix := ALStringReplaceA(LArrayPrefix, '..4-1]', '..3]');
          LArrayPrefix := ALStringReplaceA(LArrayPrefix, ']array[0..', ', 0..');
          LArrayPrefix := LArrayPrefix + ' of ';
          if LPointerPrefix <> '' then
            raise Exception.Create('Error D38ABB28-0A7C-4326-A0E1-070E4FF00D5E');
        end;
        LFieldName := CVarName2Delphi(LFieldName);
        if ALSameTextA(LFieldName, '(*unregister_module)(void)') and ALSameTextA(LCurrentType, 'void') then begin
          LFieldName := 'unregister_module';
          LCurrentType := 'TUnregisterModule';
        end
        else if ALSameTextA(LFieldName, '(*register_module)(void)') and ALSameTextA(LCurrentType, 'size_t') then begin
          LFieldName := 'register_module';
          LCurrentType := 'TRegisterModule';
        end;
        LFieldName := ALStringReplaceA(LFieldName, '$', '_');
        LLst[I] := '    ' + LFieldName + ': ' + LPointerPrefix + LArrayPrefix + LCurrentType + ';' + LComment;
        if (LPointerPrefix <> '') and (ALSameTextA(LCurrentType, LName1Str)) then
          LLst[I] := ALStringReplaceA(LLst[I], ': P_', ': P');
        LCurrentTypeUsed := True;
        inc(i);
      end;
      LValuesStr := ALTrimRight(LLst.Text);
    finally
      ALFreeAndNil(LLst);
    end;

    var LPackedSuffix: AnsiString := '';
    if LPackedRecord then LPackedSuffix := 'packed ';
    var LNoPointerDeclaration: Boolean := False;
    if (LName2Str <> '') and (ALPosA('*', LName2Str) = 1) then begin
      if LName1Str = '' then
        raise Exception.Create('Error 5C271222-7940-42D6-85D9-AAE403CA5ED7');
      delete(LName2Str, 1, 1);
      LName2Str := ALTrim(LName2Str);
      if LName2Str = '' then
        raise Exception.Create('Error 82D22228-16D2-42CA-B02C-1B2058354F0A');
      ADelphiRecords := ADelphiRecords + ALIfThenA(ADelphiRecords <> '', #13#10#13#10) +
                        //'Type' + #13#10 +
                        ALIfThenA(not LNoPointerDeclaration, '  '+LName2Str+ ' = ^'+LName1Str+';'+#13#10)+
                        ALIfThenA(not LNoPointerDeclaration, '  P'+LName2Str+ ' = ^'+LName2Str+';'+#13#10)+
                        '  ' + LName1Str + ' = '+LPackedSuffix+'record' + #13#10 +
                        LValuesStr + ALIfThenA(LValuesStr <> '', #13#10) +
                        '  end;';
    end
    else begin
      var LNameStr := LName2Str;
      If LNameStr = '' then begin
        LNameStr := LName1Str;
        if ALSameTextA(LNameStr, '_Image') then begin
          LNoPointerDeclaration := True;
          LNameStr := 'Image'; // Forward declaration
        end
        else if ALSameTextA(LNameStr, '_ImageInfo') then begin
          LNoPointerDeclaration := True;
          LNameStr := 'ImageInfo'; // Forward declaration
        end
        else if ALSameTextA(LNameStr, '_ExceptionInfo') then begin
          LNoPointerDeclaration := True;
          LNameStr := 'ExceptionInfo'; // Forward declaration
        end;
      end;
      if (LNameStr = 'ImageView') or
         (LNameStr = 'WandView') then LNoPointerDeclaration := True;
      if LNameStr = '' then
        raise Exception.Create('Error 7DAABCC6-578E-4E14-8A9E-8E06D3DA00C9');
      ADelphiRecords := ADelphiRecords + ALIfThenA(ADelphiRecords <> '', #13#10#13#10) +
                        //'Type' + #13#10 +
                        ALIfThenA(not LNoPointerDeclaration, '  P'+LNameStr+ ' = ^'+LNameStr+';'+#13#10)+
                        ALIfThenA(not LNoPointerDeclaration, '  PP'+LNameStr+ ' = ^P'+LNameStr+';'+#13#10)+
                        '  ' + LNameStr + ' = '+LPackedSuffix+'record' + #13#10 +
                        LValuesStr + ALIfThenA(LValuesStr <> '', #13#10) +
                        '  end;';
    end;

    P1 := ALPosIgnoreCaseA(' struct ', LCSrc, P4);
  end;

end;

{*************************}
procedure Extractfunctions(
            Const ACFilename: String;
            var ADelphiFunctionsObjectInterface: AnsiString;
            var ADelphiFunctionsInterface: AnsiString;
            var ADelphiFunctionsImplementation: AnsiString;
            var ADelphiFunctionsInitialization: AnsiString);
begin

  var LIsMagickCoreUnit := ALposIgnoreCaseW('\MagickCore\', ACFilename) > 0;
  var LIsMagickWandUnit := ALposIgnoreCaseW('\MagickWand\', ACFilename) > 0;
  if (not LIsMagickCoreUnit) and (not LIsMagickWandUnit) then
    Raise Exception.Create('Error F19F648F-E01B-4AC6-A6DD-7353A39A614B');
  if (LIsMagickCoreUnit) and (LIsMagickWandUnit) then
    Raise Exception.Create('Error CB6AC795-D03C-438A-9253-EF303B21A210');
  var LRegionSpecifier: AnsiString := '';

  //
  // MagickExport MagickBooleanType InterpolatePixelChannel(
  //   const Image *magick_restrict image,const CacheView_ *image_view,
  //   const PixelChannel channel,const PixelInterpolateMethod method,const double x,
  //   const double y,double *pixel,ExceptionInfo *exception)
  // {
  //   ...
  // }
  //

  var LCSrc := ALGetStringFromFile(ACFilename);
  LCSrc := ALStringReplaceA(LCSrc, #9, ' ');
  LCSrc := ALStringReplaceA(LCSrc, #13, '');
  LCSrc := ALStringReplaceA(LCSrc, #10, #13#10);
  LCSrc := ALStringReplaceA(LCSrc, 'signed int', 'signed_int');
  LCSrc := ALStringReplaceA(LCSrc, 'signed short', 'signed_short');
  LCSrc := ALStringReplaceA(LCSrc, 'unsigned char', 'unsigned_char');
  LCSrc := ALStringReplaceA(LCSrc, 'unsigned short', 'unsigned_short');
  LCSrc := ALStringReplaceA(LCSrc, 'unsigned short int', 'unsigned_short_int');
  LCSrc := ALStringReplaceA(LCSrc, 'unsigned int', 'unsigned_int');
  LCSrc := ALStringReplaceA(LCSrc, 'unsigned long', 'unsigned_long');
  LCSrc := ALStringReplaceA(LCSrc, 'long double', 'long_double');
  LCSrc := ALStringReplaceA(LCSrc, ' magick_hot_spot ', ' ');
  LCSrc := ALStringReplaceA(LCSrc, '*magick_restrict ', '*');
  LCSrc := ALStringReplaceA(LCSrc, 'void *(*relinquish_value)(void *)', 'RelinquishValueFunc relinquish_value');
  LCSrc := ALStringReplaceA(LCSrc, 'void *(*relinquish_key)(void *)', 'RelinquishKeyFunc relinquish_key');
  LCSrc := ALStringReplaceA(LCSrc, 'void *(*clone_key)(void *)', 'CloneKeyFunc clone_key');
  LCSrc := ALStringReplaceA(LCSrc, 'void *(*clone_value)(void *)', 'CloneValueFunc clone_value');
  LCSrc := ALStringReplaceA(LCSrc, 'int (*compare)(const void *,const void *)', 'SplayTreeCompareFunc compare');
  LCSrc := ALStringReplaceA(LCSrc, 'int (*compare)(const void *,const void *)', 'LinkedListCompareFunc compare');
  LCSrc := ALStringReplaceA(LCSrc, 'void (*destructor)(void *)', 'MagickThreadDestructor destructor');
  LCSrc := ALStringReplaceA(LCSrc, 'DecodeImageHandler *GetImageDecoder', 'DecodeImageHandler GetImageDecoder');
  LCSrc := ALStringReplaceA(LCSrc, 'EncodeImageHandler *GetImageEncoder', 'EncodeImageHandler GetImageEncoder');
  LCSrc := ALStringReplaceA(LCSrc, 'AcquireMemoryHandler *acquire_memory_handler','AcquireMemoryHandler acquire_memory_handler');
  LCSrc := ALStringReplaceA(LCSrc, 'ResizeMemoryHandler *resize_memory_handler','ResizeMemoryHandler resize_memory_handler');
  LCSrc := ALStringReplaceA(LCSrc, 'DestroyMemoryHandler *destroy_memory_handler','DestroyMemoryHandler destroy_memory_handler');
  LCSrc := ALStringReplaceA(LCSrc, 'WandExport','MagickExport');

  While ALPosA(' * ',LCSrc) > 0 do
    LCSrc := ALStringReplaceA(LCSrc, ' * ',' *');

  LCSrc := ALStringReplaceA(LCSrc, 'wand_unused(','magick_unused(');

  // MagickExport MagickBooleanType QueryColorname(
  //   const Image *magick_unused(image),const PixelInfo *color,
  //   const ComplianceType compliance,char *name,ExceptionInfo *exception)
  var P1 := ALposIgnoreCaseA('magick_unused(', LCSrc);
  While P1 > 0 do begin
    delete(LCSrc, P1, length('magick_unused('));
    P1 := AlposA(')', LCSrc, P1);
    if P1 <= 0 then
      Raise Exception.Create('Error B6792463-4A99-4102-8548-BD1251314630');
    delete(LCSrc, P1, 1);
    P1 := ALposIgnoreCaseA('magick_unused(', LCSrc, P1);
  end;

  LCSrc := RemoveSection(LCSrc, 'defined(MAGICKCORE_X11_DELEGATE)');
  LCSrc := RemoveSection(LCSrc, '!defined(MAGICKCORE_CIPHER_SUPPORT)');
  LCSrc := RemoveSection(LCSrc, '!defined(MAGICKCORE_ZLIB_DELEGATE)');
  LCSrc := RemoveSection(LCSrc, '!defined(MAGICKCORE_HAVE_DISTRIBUTE_CACHE)');
  LCSrc := RemoveSection(LCSrc, '!defined(MAGICKCORE_MODULES_SUPPORT)');
  LCSrc := RemoveSection(LCSrc, '!defined(MAGICKCORE_OPENCL_SUPPORT)');
  LCSrc := RemoveSection(LCSrc, '!defined(MAGICKCORE_LQR_DELEGATE)');

  P1 := ALPosIgnoreCaseA('MagickExport ', LCSrc);
  While P1 > 0 do begin

    inc(P1, length('MagickExport '));
    var P2 := ALPosA(')', LCSrc, P1);
    If P2 <= 0 then
      raise Exception.Create('Error 785718C9-ED70-4280-906F-5780D8E246EB');
    var LFctCHeader := ALCopyStr(LCSrc, P1, P2-P1+1);

    var P3 := AlposA('(', LFctCHeader);
    if P3 <= 0 then
      raise Exception.Create('Error 81564749-F5DE-4797-9E61-F99F1B033194');

    var LFctName: AnsiString := '';
    var LFctResult: AnsiString := '';
    var LFctIsVarArgs: Boolean := False;
    var LFctIsProcedure: Boolean := False;

    Var LLst := TALStringListA.Create;
    try
      LLst.LineBreak := ' ';
      LLst.Text := ALTrim(AlCopyStr(LFctCHeader, 1, P3-1));

      if LLst.Count = 0 then
        raise Exception.Create('Error DF5C9FE2-2EB6-420E-8732-7CF2653CD800');
      if AlsameTextA(AlTrim(LLst[0]), 'const') then LLst.Delete(0);
      if LLst.Count = 0 then
        raise Exception.Create('Error A040A65D-5E16-4078-8737-9AA949F0A98E');
      if AlsameTextA(AlTrim(LLst[0]), 'struct') then LLst.Delete(0);
      if LLst.Count <> 2 then
        raise Exception.Create('Error 525A5E29-5612-4EC7-B648-85C062FE0A42');

      LFctResult := ALTrim(LLst[0]);
      if LFctResult = '' then
        Raise exception.Create('Error 96B7C128-81AB-443D-9D09-488ABB95087A');
      if ALSameTextA(LFctResult, 'void') then LFctIsProcedure := True;
      LFctResult := CType2Delphi(LFctResult);

      LFctName := ALTrim(LLst[1]);
      if LFctName = '' then
        Raise exception.Create('Error E35B938E-D9A4-436E-B1C2-0594D2B3CF3B');
      If AlPosA('***', LFctName) = 1 then begin
        delete(LFctName, 1, 3);
        LFctResult := 'PPP' + LFctResult;
        if length(LFctResult) <= 3 then
          raise Exception.Create('Error 0DB42C44-CA45-42E9-BCBB-E39BE0EEB540');
        LFctResult[4] := ALUpCase(LFctResult[4]);
      end
      else If AlPosA('**', LFctName) = 1 then begin
        delete(LFctName, 1, 2);
        LFctResult := 'PP' + LFctResult;
        if length(LFctResult) <= 2 then
          raise Exception.Create('Error 9BFA51AA-0C2E-4B79-ACBF-4842773F9DF8');
        LFctResult[3] := ALUpCase(LFctResult[3]);
      end
      else If AlPosA('*', LFctName) = 1 then begin
        delete(LFctName, 1, 1);
        LFctResult := 'P' + LFctResult;
        if length(LFctResult) <= 1 then
          raise Exception.Create('Error 4FF71CE7-A9A0-41B6-8939-060651422D76');
        LFctResult[2] := ALUpCase(LFctResult[2]);
      end;

    finally
      ALFreeAndNil(LLst);
    end;

    var LCParamsLst := TALStringListA.Create;
    var LDelphiParamsLst := TALStringListA.Create;
    Try
      LCParamsLst.LineBreak := ',';
      LCParamsLst.Text := AlCopyStr(LFctCHeader, P3+1, length(LFctCHeader) - P3 - 1);
      LDelphiParamsLst.LineBreak := '; ';
      LDelphiParamsLst.NameValueSeparator := ':';
      LDelphiParamsLst.TrailingLineBreak := False;
      for var I := 0 to LCParamsLst.Count - 1 do begin

        var LLine := ALTrim(LCParamsLst[I]);
        If alsameTextA(LLine, 'void') then begin
          if (I <> 0) or (LCParamsLst.Count <> 1) then
            Raise Exception.Create('Error C3D51E9C-05E0-4942-98CE-78DD9FF9731B');
          Break;
        end;

        var LCParamLst := TALStringListA.Create;
        try

          LCParamLst.LineBreak := ' ';
          LCParamLst.Text := LLine;
          if LCParamLst.Count = 0 then
            raise Exception.Create('Error E076C2F7-A5F1-48E5-9788-AA8311050B91');
          var LDelphiParam: AnsiString := '';
          if AlsameTextA(AlTrim(LCParamLst[0]), 'const') then begin
            LDelphiParam := 'const ';
            LCParamLst.Delete(0);
          end;
          if (LCParamLst.Count = 1) and (LCParamLst[0] = '...') then begin
            // http://rvelthuis.de/articles/articles-convert.html#varargs
            if I <> LCParamsLst.Count - 1 then
              Raise Exception.Create('Error F6B08484-F417-4F62-8FB5-718D7A55BD66');
            LFctIsVarArgs := True;
            continue;
          end;
          if LCParamLst.Count <> 2 then
            raise Exception.Create('Error 21687FF3-55A2-4CE2-B239-1E73202E47D4');

          var LParamType := ALTrim(LCParamLst[0]);
          if LParamType = '' then
            Raise exception.Create('Error 96B7C128-81AB-443D-9D09-488ABB95087A');
          LParamType := CType2Delphi(LParamType);

          var LParamName := ALTrim(LCParamLst[1]);
          if LParamName = '' then
            Raise exception.Create('Error E35B938E-D9A4-436E-B1C2-0594D2B3CF3B');
          If AlPosA('***', LParamName) = 1 then begin
            delete(LParamName, 1, 3);
            LParamType := 'PPP' + LParamType;
            if length(LParamType) <= 3 then
              raise Exception.Create('Error D1A4D807-D617-4CB2-AF55-B506261E4372');
            LParamType[4] := ALUpCase(LParamType[4]);
          end
          else If AlPosA('**', LParamName) = 1 then begin
            delete(LParamName, 1, 2);
            LParamType := 'PP' + LParamType;
            if length(LParamType) <= 2 then
              raise Exception.Create('Error 86123AE8-CC5B-4711-ADF3-7927CFD1A5EB');
            LParamType[3] := ALUpCase(LParamType[3]);
          end
          else If AlPosA('*', LParamName) = 1 then begin
            delete(LParamName, 1, 1);
            LParamType := 'P' + LParamType;
            if length(LParamType) <= 1 then
              raise Exception.Create('Error F3099FAE-AF76-4961-8C20-626E611E04F7');
            LParamType[2] := ALUpCase(LParamType[2]);
          end;
          LParamName := CVarName2Delphi(LParamName);

          LDelphiParam := LDelphiParam + LParamName + ': ' + LParamType;
          LDelphiParamsLst.Add(LDelphiParam);

        finally
          ALFreeAndNil(LCParamLst);
        end;
      end;

      var LFctHidden: AnsiString := '';
      if LFctIsVarArgs or
         ALSameTextA(LFctName, 'GetColorCompliance') or
         ALSameTextA(LFctName, 'GetLocaleValue') or
         ALSameTextA(LFctName, 'AcquireMimeCache') or
         ALSameTextA(LFctName, 'AcquireModuleInfo') or
         ALSameTextA(LFctName, 'LoadFontConfigFonts') or
         ALSameTextA(LFctName, 'MagickSetImagePixelInterpolateMethod') or
         ALSameTextA(LFctName, 'GetBlobProperties') then LFctHidden := '//';

      if LRegionSpecifier = '' then begin
        var LFilename := AnsiString(ALExtractFileName(ACFilename));
        if LIsMagickCoreUnit then LRegionSpecifier := '{$REGION ''MagickCore - '+LFilename+' - https://imagemagick.org/api/'+AlStringReplaceA(LFilename, '.c','.html')+'''}'
        else LRegionSpecifier := '{$REGION ''MagickWand - '+LFilename+' - https://imagemagick.org/api/'+AlStringReplaceA(LFilename, '.c','.html')+'''}';
        ADelphiFunctionsObjectInterface := ADelphiFunctionsObjectInterface + #13#10#13#10 + '    ' + LRegionSpecifier;
        ADelphiFunctionsInitialization := ADelphiFunctionsInitialization + #13#10#13#10 + '  ' + LRegionSpecifier;
        ADelphiFunctionsInterface := ADelphiFunctionsInterface + #13#10#13#10 + ALStringReplaceA(LRegionSpecifier, '{$REGION ''', '{$REGION '' ');
        ADelphiFunctionsImplementation := ADelphiFunctionsImplementation + #13#10#13#10 + ALStringReplaceA(LRegionSpecifier, '{$REGION ''', '{$REGION '' ');
      end;

      if LFctIsProcedure then begin
        ADelphiFunctionsObjectInterface := ADelphiFunctionsObjectInterface + #13#10 +
                                             '    ' + LFctHidden + LFctName + ': procedure(' + LDelphiParamsLst.Text + '); cdecl;';
        ADelphiFunctionsInterface := ADelphiFunctionsInterface + #13#10 +
                                     LFctHidden + 'procedure ' + LFctName + '(' + LDelphiParamsLst.Text + '); inline;';
        var LFctHeader := LFctHidden + 'procedure ' + LFctName + '(' + LDelphiParamsLst.Text + ');';
        ADelphiFunctionsImplementation := ADelphiFunctionsImplementation + #13#10#13#10 +
                                          '{' + AnsiString(StringOfChar('*', length(LFctHeader) - 2)) +'}' + #13#10 +
                                          LFctHeader + #13#10 +
                                          LFctHidden + 'begin'+#13#10 +
                                          LFctHidden + '  ALImageMagickLib.' + LFctName + '(';
        for var I := 0 to LDelphiParamsLst.Count - 1 do
          ADelphiFunctionsImplementation := ADelphiFunctionsImplementation + ALStringReplaceA(LDelphiParamsLst.Names[i], 'const ', '') + ALIfThenA(I < LDelphiParamsLst.Count - 1, ', ');
        ADelphiFunctionsImplementation := ADelphiFunctionsImplementation +');'+#13#10 +
                                          LFctHidden + 'end;';
      end
      else begin
        ADelphiFunctionsObjectInterface := ADelphiFunctionsObjectInterface + #13#10 +
                                             '    ' + LFctHidden + LFctName + ': function(' + LDelphiParamsLst.Text + '): ' + LFctResult + '; cdecl;';
        ADelphiFunctionsInterface := ADelphiFunctionsInterface + #13#10 +
                                     LFctHidden + 'function ' + LFctName + '(' + LDelphiParamsLst.Text + '): ' + LFctResult + '; inline;';
        var LFctHeader := LFctHidden + 'function ' + LFctName + '(' + LDelphiParamsLst.Text + '): ' + LFctResult + ';';
        ADelphiFunctionsImplementation := ADelphiFunctionsImplementation + #13#10#13#10 +
                                          '{' + AnsiString(StringOfChar('*', length(LFctHeader) - 2)) +'}' + #13#10 +
                                          LFctHeader + #13#10 +
                                          LFctHidden + 'begin'+#13#10+
                                          LFctHidden + '  Result := ALImageMagickLib.' + LFctName + '(';
        for var I := 0 to LDelphiParamsLst.Count - 1 do
          ADelphiFunctionsImplementation := ADelphiFunctionsImplementation + ALStringReplaceA(LDelphiParamsLst.Names[i], 'const ', '') + ALIfThenA(I < LDelphiParamsLst.Count - 1, ', ');
        ADelphiFunctionsImplementation := ADelphiFunctionsImplementation +');'+#13#10 +
                                          LFctHidden + 'end;';
      end;
      if LFctIsVarArgs then ADelphiFunctionsObjectInterface := ADelphiFunctionsObjectInterface + ' varargs;';

      ADelphiFunctionsInitialization := ADelphiFunctionsInitialization + #13#10 +
                                        '  ' + LFctHidden + LFctName + ' := GetProcAddress('+ALIfThenA(LIsMagickCoreUnit, 'FMagickCorelib','FMagickWandlib')+','''+LFctName+''');';

    Finally
      ALFreeAndNil(LCParamsLst);
      AlfreeAndNil(LDelphiParamsLst);
    End;

    P1 := ALPosIgnoreCaseA('MagickExport ', LCSrc, P1);

  end;

  if LRegionSpecifier <> '' then begin
    ADelphiFunctionsObjectInterface := ADelphiFunctionsObjectInterface + #13#10 + '    ' +'{$ENDREGION}';
    ADelphiFunctionsInitialization := ADelphiFunctionsInitialization + #13#10 + '  ' + '{$ENDREGION}';
    ADelphiFunctionsInterface := ADelphiFunctionsInterface + #13#10 + '{$ENDREGION}';
    ADelphiFunctionsImplementation := ADelphiFunctionsImplementation + #13#10#13#10 + '{$ENDREGION}';
  end;
end;

begin
  try

    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    var LImageMagickDefaultDir: string;
    var LImageMagickDir: string;
    LImageMagickDefaultDir := '..\..\..\..\ImageMagick';

    Writeln('Please enter the directory of the ImageMagick source:');
    Writeln('Cloned from https://github.com/ImageMagick/ImageMagick.git');
    Writeln('Leave empty to use the default directory: ', LImageMagickDefaultDir);
    Write('Directory: ');
    Readln(LImageMagickDir);

    if Trim(LImageMagickDir) = '' then
      LImageMagickDir := LImageMagickDefaultDir;

    var LDelphiEnumsSrc: AnsiString := '';
    var LDelphiRecordsSrc: AnsiString := '';
    var LDelphiFunctionsObjectInterface: AnsiString := '';
    var LDelphiFunctionsInterface: AnsiString := '';
    var LDelphiFunctionsImplementation: AnsiString := '';
    var LDelphiFunctionsInitialization: AnsiString := '';

    var LFiles := TArray.Concat<string>([
                    TDirectory.GetFiles(LImageMagickDir + '\MagickCore', '*.h', TSearchOption.soAllDirectories),
                    TDirectory.GetFiles(LImageMagickDir + '\MagickWand', '*.h', TSearchOption.soAllDirectories)]);
    TArray.Sort<string>(LFiles,
      TComparer<string>.Construct(
        function(const L, R: string): Integer
          function GetRank(const S: String): Integer;
          begin
            if ALSameTextW(S, 'magick-type.h')  then Result := 0
            else if ALSameTextW(S, 'pixel.h')  then Result := 1
            else if ALSameTextW(S, 'geometry.h')  then Result := 2
            else if ALSameTextW(S, 'blob.h')  then Result := 3
            else if ALSameTextW(S, 'timer.h')  then Result := 4
            else if ALSameTextW(S, 'color.h')  then Result := 5
            else if ALSameTextW(S, 'compress.h')  then Result := 6
            else if ALSameTextW(S, 'semaphore.h')  then Result := 7
            else if ALSameTextW(S, 'profile.h')  then Result := 8
            else if ALSameTextW(S, 'image.h')  then Result := 9
            else Result := 10000;
          end;
        begin
          Result := GetRank(ALExtractFilename(L)) - GetRank(ALExtractFilename(R));
        end));

    for var LFile in LFiles do begin
      var LFilename := ALExtractFilename(LFile);
      If (ALPosIgnoreCaseA('-private.h', AnsiString(LFilename)) > 0) and (not ALSameTextW(Lfilename, 'blob-private.h')) then continue;
      If ALSameTextW(Lfilename, 'deprecate.h') then continue;
      If ALSameTextW('vms.h', LFilename) then continue;
      ExtractEnums(LFile, LDelphiEnumsSrc);
    end;

    for var LFile in LFiles do begin
      var LFilename := ALExtractFilename(LFile);
      If ALPosIgnoreCaseA('-private.h', AnsiString(LFilename)) > 0 then continue;
      If ALSameTextW(Lfilename, 'deprecate.h') then continue;
      If ALSameTextW('vms.h', LFilename) then continue;
      ExtractRecords(LFile, LDelphiRecordsSrc);
    end;

    LFiles := TArray.Concat<string>([
                TDirectory.GetFiles(LImageMagickDir + '\MagickCore', '*.c', TSearchOption.soAllDirectories),
                TDirectory.GetFiles(LImageMagickDir + '\MagickWand', '*.c', TSearchOption.soAllDirectories)]);

    for var LFile in LFiles do begin
      var LFilename := ALExtractFilename(LFile);
      If ALPosIgnoreCaseA('-private.c', AnsiString(LFilename)) > 0 then continue;
      If ALSameTextW(Lfilename, 'deprecate.c') then continue;
      If ALSameTextW('vms.c', LFilename) then continue;
      Extractfunctions(LFile, LDelphiFunctionsObjectInterface, LDelphiFunctionsInterface, LDelphiFunctionsImplementation, LDelphiFunctionsInitialization);
    end;

    var LAlcinoeImageMagickSrc: AnsiString := '''
          unit Alcinoe.ImageMagick;

          interface

          {$I Alcinoe.inc}
          {$SCOPEDENUMS OFF}

          //
          // https://www.imagemagick.org/script/magick-wand.php
          // https://www.imagemagick.org/MagickWand/
          //
          // This wrapper was automagically generated from ImageMagick version 7.1.2-8
          // using Alcinoe\Tools\ImageMagickWrapperGenerator\.
          // DLLs: /Libraries/dll/ImageMagick
          //
          // IMPORTANT:
          // Whenever the ImageMagick version is updated, this unit must be regenerated
          // with Alcinoe\Tools\ImageMagickWrapperGenerator\ to stay in sync with the C API.
          //

          {$MINENUMSIZE 4} // https://stackoverflow.com/questions/48953749/why-this-c-to-pascal-conversion-crash

          uses
            winapi.windows;

          {$IF (not defined(MAGICKCORE_QUANTUM_DEPTH_8)) and (not defined(MAGICKCORE_QUANTUM_DEPTH_16)) and (not defined(MAGICKCORE_QUANTUM_DEPTH_32)) and (not defined(MAGICKCORE_QUANTUM_DEPTH_64))}
            {$define MAGICKCORE_QUANTUM_DEPTH_16}
          {$ENDIF}

          {$IF not defined(MAGICKCORE_NO_HDRI_SUPPORT)}
            {$define MAGICKCORE_HDRI_SUPPORT}
          {$ENDIF}

          {$IF defined(WIN64)}
            {$define MAGICKCORE_64BIT_CHANNEL_MASK_SUPPORT}
          {$ENDIF}

          type
            MagickFloatType = single; // float
            MagickDoubleType = double;

            {$IF defined(MAGICKCORE_QUANTUM_DEPTH_8)}
              {$IF defined(MAGICKCORE_HDRI_SUPPORT)}
                Quantum = MagickFloatType;
              {$ELSE}
                Quantum = Byte; // unsigned char
              {$ENDIF}
            {$ELSEIF defined(MAGICKCORE_QUANTUM_DEPTH_16)}
              {$IF defined(MAGICKCORE_HDRI_SUPPORT)}
                Quantum = MagickFloatType;
              {$ELSE}
                Quantum = Word; // unsigned short
              {$ENDIF}
            {$ELSEIF defined(MAGICKCORE_QUANTUM_DEPTH_32)}
              {$IF defined(MAGICKCORE_HDRI_SUPPORT)}
                Quantum = MagickDoubleType;
              {$ELSE}
                Quantum = Cardinal; // unsigned int
              {$ENDIF}
            {$ELSEIF defined(MAGICKCORE_QUANTUM_DEPTH_64)}
              Quantum = MagickDoubleType;
            {$ELSE}
              error "MAGICKCORE_QUANTUM_DEPTH must be one of 8, 16, 32, or 64"
            {$ENDIF}
            PQuantum = ^Quantum;

            MagickRealType = MagickDoubleType;
            PMagickRealType = ^MagickRealType;
            MagickStatusType = Cardinal; // unsigned int
            MagickOffsetType = Int64; // __int64
            PMagickOffsetType = ^MagickOffsetType;
            MagickSizeType = UInt64; // unsigned __int64
            PMagickSizeType = ^MagickSizeType;
            MagickThreadKey = DWORD; // typedef DWORD MagickThreadKey;
            PMagickThreadKey = ^MagickThreadKey;
            QuantumAny = MagickSizeType;
            PFILE = Pointer;
            PPDouble = ^PDouble;
            PPPAnsiChar = ^PPAnsiChar;

          const
            MaxPixelChannels = 64; // #define MaxPixelChannels  64
            MaximumNumberOfImageMoments = 8; // #define MaximumNumberOfImageMoments  8
            MaximumNumberOfPerceptualColorspaces = 6; // #define MaximumNumberOfPerceptualColorspaces  6
            MaximumNumberOfPerceptualHashes = 7; // #define MaximumNumberOfPerceptualHashes  7
            MagickPathExtent = 4096; // #define MagickPathExtent  4096
            CCMaxMetrics = 16; // #define CCMaxMetrics  16

          type
            time_t = Int64;
            TUnregisterModule = procedure; cdecl;
            TRegisterModule = function: NativeUInt; cdecl;

            %s

            PImage = ^Image;
            PPImage = ^PImage;
            PImageInfo = ^ImageInfo;
            PPImageInfo = ^PImageInfo;
            PImageView = ^ImageView;
            PPImageView = ^PImageView;
            PWandView = ^WandView;
            PPWandView = ^PWandView;
            PExceptionInfo = ^ExceptionInfo;
            PPExceptionInfo = ^PExceptionInfo;

            // void *(*relinquish_value)(void *)
            RelinquishValueFunc = function(value: PVoid): PVoid; cdecl;

            // void *(*relinquish_key)(void *)
            RelinquishKeyFunc = function(value: PVoid): PVoid; cdecl;

            // void *(*clone_key)(void *)
            CloneKeyFunc = function(value: PVoid): PVoid; cdecl;

            // void *(*clone_value)(void *)
            CloneValueFunc = function(value: PVoid): PVoid; cdecl;

            // int (*compare)(const void *,const void *)
            SplayTreeCompareFunc = function(const a: PVoid; const b: PVoid): Integer; cdecl;

            // int (*compare)(const void *,const void *)
            LinkedListCompareFunc = function(const a: PVoid; const b: PVoid): Integer; cdecl;

            // void (*destructor)(void *)
            MagickThreadDestructor = procedure(context: PVoid); cdecl;

            // typedef MagickBooleanType
            //   (*MagickProgressMonitor)(const char *,const MagickOffsetType,
            //     const MagickSizeType,void *);
            MagickProgressMonitor = function(
                                      const text: PAnsiChar;
                                      const offset: MagickOffsetType;
                                      const size: MagickSizeType;
                                      context: PVoid): MagickBooleanType; cdecl;

            // typedef size_t
            //   (*StreamHandler)(const Image *,const void *,const size_t);
            StreamHandler = function(
                              const image: PImage;
                              const data: PVoid;
                              const length: size_t): size_t; cdecl;

            // typedef Image
            //   *DecodeImageHandler(const ImageInfo *,ExceptionInfo *);
            DecodeImageHandler = function(
                                   const image_info: PImageInfo;
                                   exception: PExceptionInfo): PImage; cdecl;

            // typedef MagickBooleanType
            //   EncodeImageHandler(const ImageInfo *,Image *,ExceptionInfo *);
            EncodeImageHandler = function(
                                   const image_info: PImageInfo;
                                   image: PImage;
                                   exception: PExceptionInfo): MagickBooleanType; cdecl;

            // typedef MagickBooleanType
            //   IsImageFormatHandler(const unsigned char *,const size_t);
            IsImageFormatHandler = function(
                                     const data: PByte;
                                     const length: size_t): MagickBooleanType; cdecl;

            // typedef ssize_t
            //   (*CustomStreamHandler)(unsigned char *,const size_t,void *);
            CustomStreamHandler = function(
                                    buffer: PByte;
                                    const length: size_t;
                                    context: PVoid): ssize_t; cdecl;

            // typedef MagickOffsetType
            //   (*CustomStreamSeeker)(const MagickOffsetType,const int,void *);
            CustomStreamSeeker = function(
                                   const offset: MagickOffsetType;
                                   const whence: Integer;
                                   context: PVoid): MagickOffsetType; cdecl;

            // typedef MagickOffsetType
            //   (*CustomStreamTeller)(void *);
            CustomStreamTeller = function(
                                   context: PVoid): MagickOffsetType; cdecl;

            // typedef void
            //   (*ErrorHandler)(const ExceptionType,const char *,const char *);
            ErrorHandler = procedure(
                             const error_type: ExceptionType;
                             const reason: PAnsiChar;
                             const description: PAnsiChar); cdecl;

            // typedef void
            //   (*FatalErrorHandler)(const ExceptionType,const char *,const char *)
            //     magick_attribute((__noreturn__));
            FatalErrorHandler = procedure(
                                  const error_type: ExceptionType;
                                  const reason: PAnsiChar;
                                  const description: PAnsiChar); cdecl;

            // typedef void
            //   (*WarningHandler)(const ExceptionType,const char *,const char *);
            WarningHandler = procedure(
                               const error_type: ExceptionType;
                               const reason: PAnsiChar;
                               const description: PAnsiChar); cdecl;

            // typedef MagickBooleanType
            //   (*DuplexTransferImageViewMethod)(const ImageView *,const ImageView *,
            //     ImageView *,const ssize_t,const int,void *);
            DuplexTransferImageViewMethod = function(
                                              const source_view: PImageView;
                                              const duplex_view: PImageView;
                                              destination_view: PImageView;
                                              const y: ssize_t;
                                              const thread_id: Integer;
                                              context: PVoid): MagickBooleanType; cdecl;

            // typedef MagickBooleanType
            //   (*GetImageViewMethod)(const ImageView *,const ssize_t,const int,void *);
            GetImageViewMethod = function(
                                   const source_view: PImageView;
                                   const y: ssize_t;
                                   const thread_id: Integer;
                                   context: PVoid): MagickBooleanType; cdecl;

            // typedef MagickBooleanType
            //   (*SetImageViewMethod)(ImageView *,const ssize_t,const int,void *);
            SetImageViewMethod = function(
                                   destination_view: PImageView;
                                   const y: ssize_t;
                                   const thread_id: Integer;
                                   context: PVoid): MagickBooleanType; cdecl;

            // typedef MagickBooleanType
            //   (*TransferImageViewMethod)(const ImageView *,ImageView *,const ssize_t,
            //     const int,void *);
            TransferImageViewMethod = function(
                                        const source_view: PImageView;
                                        destination_view: PImageView;
                                        const y: ssize_t;
                                        const thread_id: Integer;
                                        context: PVoid): MagickBooleanType; cdecl;

            // typedef MagickBooleanType
            //   (*UpdateImageViewMethod)(ImageView *,const ssize_t,const int,void *);
            UpdateImageViewMethod = function(
                                      view: PImageView;
                                      const y: ssize_t;
                                      const thread_id: Integer;
                                      context: PVoid): MagickBooleanType; cdecl;

            // typedef void
            //   (*MagickLogMethod)(const LogEventType,const char *);
            MagickLogMethod = procedure(
                                const event_type: LogEventType;
                                const message: PAnsiChar); cdecl;

            // typedef void
            //   *(*AcquireMemoryHandler)(size_t) magick_alloc_size(1);
            AcquireMemoryHandler = procedure(size: size_t); cdecl;

            // typedef void
            //   (*DestroyMemoryHandler)(void *);
            DestroyMemoryHandler = procedure(memory: PVoid); cdecl;

            // typedef void
            //   *(*ResizeMemoryHandler)(void *,size_t) magick_alloc_size(2);
            ResizeMemoryHandler = procedure(
                                    memory: PVoid;
                                    size: size_t); cdecl;

            // typedef void
            //   *(*AcquireAlignedMemoryHandler)(const size_t,const size_t);
            AcquireAlignedMemoryHandler = procedure(
                                            const alignment: size_t;
                                            const size: size_t); cdecl;

            // typedef void
            //   (*RelinquishAlignedMemoryHandler)(void *);
            RelinquishAlignedMemoryHandler = procedure(memory: PVoid); cdecl;

            //typedef MagickBooleanType
            //  (*MagickCommand)(ImageInfo *,int,char **,char **,ExceptionInfo *);
            MagickCommand = function(
                              image_info: PImageInfo;
                              argc: Integer;
                              argv: PPAnsiChar;
                              metadata: PPAnsiChar;
                              exception: PExceptionInfo): MagickBooleanType; cdecl;

            // typedef MagickBooleanType
            //   (*DuplexTransferWandViewMethod)(const WandView *,const WandView *,WandView *,
            //     const ssize_t,const int,void *),
            DuplexTransferWandViewMethod = function(
                                             const source_view1: PWandView;
                                             const source_view2: PWandView;
                                             destination_view: PWandView;
                                             const y: ssize_t;
                                             const thread_id: Integer;
                                             context: PVoid): MagickBooleanType; cdecl;

            // typedef MagickBooleanType
            //   (*GetWandViewMethod)(const WandView *,const ssize_t,const int,void *),
            GetWandViewMethod = function(
                                  const source_view: PWandView;
                                  const y: ssize_t;
                                  const thread_id: Integer;
                                  context: PVoid): MagickBooleanType; cdecl;

            // typedef MagickBooleanType
            //   (*SetWandViewMethod)(WandView *,const ssize_t,const int,void *),
            SetWandViewMethod = function(
                                  destination_view: PWandView;
                                  const y: ssize_t;
                                  const thread_id: Integer;
                                  context: PVoid): MagickBooleanType; cdecl;

            // typedef MagickBooleanType
            //   (*TransferWandViewMethod)(const WandView *,WandView *,const ssize_t,
            //     const int,void *),
            TransferWandViewMethod = function(
                                       const source_view: PWandView;
                                       destination_view: PWandView;
                                       const y: ssize_t;
                                       const thread_id: Integer;
                                       context: PVoid): MagickBooleanType; cdecl;

            // typedef MagickBooleanType
            //   (*UpdateWandViewMethod)(WandView *,const ssize_t,const int,void *);
            UpdateWandViewMethod = function(
                                     destination_view: PWandView;
                                     const y: ssize_t;
                                     const thread_id: Integer;
                                     context: PVoid): MagickBooleanType; cdecl;

            %s

            TALImageMagickLibrary = class(TObject)
            private
              FMagickCorelib: THandle;
              FMagickWandlib: THandle;
            public

              %s

            public
              constructor Create(AImageMagickHome: String; const AThreadLimit: integer = 0); virtual;
              destructor Destroy; override;
            end;

          Var
            ALImageMagickLib: TALImageMagickLibrary;

          %s

          procedure ALCreateImageMagickLibrary(const AImageMagickHome: String; const AThreadLimit: integer = 0);
          procedure ALFreeImageMagickLibrary;
          procedure ALRaiseLastMagickWandError(const AWand: PMagickWand);
          procedure ALRaiseLastPixelWandError(const AWand: PPixelWand);
          procedure ALRaiseLastDrawingWandError(const AWand: PDrawingWand);
          procedure ALCheckMagickWandResult(const AResult: MagickBooleanType; const AWand: PMagickWand); overload;
          function  ALCheckMagickWandResult(const AResult: PMagickWand): PMagickWand; overload;
          procedure ALCheckPixelWandResult(const AResult: MagickBooleanType; const AWand: PPixelWand); overload;
          function  ALCheckPixelWandResult(const AResult: PPixelWand): PPixelWand; overload;
          procedure ALCheckDrawingWandResult(const AResult: MagickBooleanType; const AWand: PDrawingWand); overload;
          function  ALCheckDrawingWandResult(const AResult: PDrawingWand): PDrawingWand; overload;

          implementation

          uses
            system.IOUtils,
            system.sysutils,
            Alcinoe.Common;

          {*************************}
          {$WARN SYMBOL_PLATFORM OFF}
          function _wputenv_s(varname: PChar; value_string: PChar): integer; cdecl; external 'ucrtbase.dll' delayed;
          {$WARN SYMBOL_PLATFORM ON}

          {**************************************************************************************************}
          constructor TALImageMagickLibrary.Create(AImageMagickHome: String; const AThreadLimit: integer = 0);

            {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
            function GetProcAddress(const AModule: HMODULE; const AProcName: PAnsiChar): Pointer;
            begin
              Result := Winapi.Windows.GetProcAddress(AModule, AProcName);
              if Result = nil then
                raise Exception.CreateFmt('Function "%%s" was not found in the ImageMagick DLL', [AProcName]);
            end;

          begin

            // http://www.imagemagick.org/script/resources.php
            AImageMagickHome := ExcludeTrailingPathDelimiter(AImageMagickHome);
            var LPath := getEnvironmentVariable('PATH');
            if ((pos(AImageMagickHome, LPath) <= 0) and
                (not setEnvironmentVariable(PChar('PATH'), pChar(LPath + ';' + AImageMagickHome)))) then raiseLastOsError;

            // http://www.imagemagick.org/script/resources.php
            // https://stackoverflow.com/questions/69199708/setenvironmentvariable-does-not-seem-to-set-values-that-can-be-retrieved-by-ge
            // https://stackoverflow.com/questions/69199952/how-to-call-putenv-from-delphi/69200498
            if (_wputenv_s(PChar('MAGICK_HOME'), pChar(AImageMagickHome)) <> 0) or
               (_wputenv_s(PChar('MAGICK_CONFIGURE_PATH'), pChar(AImageMagickHome)) <> 0) or
               (_wputenv_s(PChar('MAGICK_CODER_FILTER_PATH'), pChar(TPath.Combine(AImageMagickHome, 'modules\filters'))) <> 0) or
               (_wputenv_s(PChar('MAGICK_CODER_MODULE_PATH'), pChar(TPath.Combine(AImageMagickHome, 'modules\coders'))) <> 0) then raiseLastOsError;

            //https://www.imagemagick.org/discourse-server/viewtopic.php?f=6&t=33662
            //https://stackoverflow.com/questions/49266246/imagemagick-wand-use-only-one-single-cpu
            if (AThreadLimit > 0) and
               (_wputenv_s(PChar('MAGICK_THREAD_LIMIT'), pChar(Inttostr(AThreadLimit))) <> 0) then raiseLastOsError;

            FMagickCorelib := LoadLibrary(pChar(TPath.Combine(AImageMagickHome, 'CORE_RL_MagickCore_.dll')));
            if FMagickCorelib = 0 then raiseLastOsError;

            FMagickWandlib := LoadLibrary(pChar(TPath.Combine(AImageMagickHome, 'CORE_RL_MagickWand_.dll')));
            if FMagickWandlib = 0 then raiseLastOsError;

            %s

            MagickWandGenesis;

          end;

          {***************************************}
          destructor TALImageMagickLibrary.Destroy;
          begin
            if FMagickWandlib > 0 then begin
              MagickWandTerminus;
              // After heavy ImageMagick usage, immediately unloading the DLL can
              // occasionally trigger an AV inside FreeLibrary(FMagickWandlib).
              // Pause briefly to let internal threads/finalizers settle before freeing.
              sleep(250);
              if not FreeLibrary(FMagickWandlib) then
                raiseLastOsError;
            end;
            if (FMagickCorelib > 0) and
               (not FreeLibrary(FMagickCorelib)) then
              raiseLastOsError;
            inherited Destroy;
          end;

          %s

          {*************************************************************}
          procedure ALRaiseLastMagickWandError(const AWand: PMagickWand);
          begin
            var LSeverity: ExceptionType;
            var LPAnsiChar: PAnsiChar := ALImageMagickLib.MagickGetException(AWand, @LSeverity);
            var LDescription: ansiString := LPAnsiChar;
            ALImageMagickLib.MagickRelinquishMemory(LPAnsiChar);
            raise Exception.create(string(LDescription));
          end;

          {***********************************************************}
          procedure ALRaiseLastPixelWandError(const AWand: PPixelWand);
          begin
            var LSeverity: ExceptionType;
            var LPAnsiChar: PAnsiChar := ALImageMagickLib.PixelGetException(AWand, @LSeverity);
            var LDescription: ansiString := LPAnsiChar;
            ALImageMagickLib.MagickRelinquishMemory(LPAnsiChar);
            raise Exception.create(string(LDescription));
          end;

          {***************************************************************}
          procedure ALRaiseLastDrawingWandError(const AWand: PDrawingWand);
          begin
            var LSeverity: ExceptionType;
            var LPAnsiChar: PAnsiChar := ALImageMagickLib.DrawGetException(AWand, @LSeverity);
            var LDescription: ansiString := LPAnsiChar;
            ALImageMagickLib.MagickRelinquishMemory(LPAnsiChar);
            raise Exception.create(string(LDescription));
          end;

          {********************************************************************************************}
          procedure ALCheckMagickWandResult(const AResult: MagickBooleanType; const AWand: PMagickWand);
          begin
            if AResult <> MagickTrue then ALRaiseLastMagickWandError(AWand);
          end;

          {************************************************************************}
          function ALCheckMagickWandResult(const AResult: PMagickWand): PMagickWand;
          begin
            if AResult = Nil then raise Exception.create('MagickWand operation failed: returned a nil wand pointer');
            Result := AResult;
          end;

          {******************************************************************************************}
          procedure ALCheckPixelWandResult(const AResult: MagickBooleanType; const AWand: PPixelWand);
          begin
            if AResult <> MagickTrue then ALRaiseLastPixelWandError(AWand);
          end;

          {*********************************************************************}
          function ALCheckPixelWandResult(const AResult: PPixelWand): PPixelWand;
          begin
            if AResult = nil then raise Exception.create('PixelWand operation failed: returned a nil wand pointer');
            Result := AResult;
          end;

          {**********************************************************************************************}
          procedure ALCheckDrawingWandResult(const AResult: MagickBooleanType; const AWand: PDrawingWand);
          begin
            if AResult <> MagickTrue then ALRaiseLastDrawingWandError(AWand);
          end;

          {***************************************************************************}
          function ALCheckDrawingWandResult(const AResult: PDrawingWand): PDrawingWand;
          begin
            if AResult = nil then raise Exception.create('DrawingWand operation failed: returned a nil wand pointer');
            Result := AResult;
          end;

          {****************************************************************************************************}
          procedure ALCreateImageMagickLibrary(const AImageMagickHome: String; const AThreadLimit: integer = 0);
          begin
            if assigned(ALImageMagickLib) then exit;
            ALImageMagickLib := TALImageMagickLibrary.Create(AImageMagickHome, AThreadLimit);
          end;

          {*********************************}
          procedure ALFreeImageMagickLibrary;
          begin
            alFreeAndNil(ALImageMagickLib);
          end;

          initialization
            {$IF defined(DEBUG)}
            ALLog('Alcinoe.ImageMagick','initialization');
            {$ENDIF}
            ALImageMagickLib := nil;

          end.
          ''';

    LAlcinoeImageMagickSrc := ALFormatA(
                                LAlcinoeImageMagickSrc,
                                [ALTrim(LDelphiEnumsSrc),
                                 ALTrim(LDelphiRecordsSrc),
                                 ALTrim(LDelphiFunctionsObjectInterface),
                                 ALTrim(LDelphiFunctionsInterface),
                                 ALTrim(LDelphiFunctionsInitialization),
                                 ALTrim(LDelphiFunctionsImplementation)]);

    ALSaveStringToFile(LAlcinoeImageMagickSrc, ALGetModulePathW + '..\..\Source\Alcinoe.ImageMagick.pas');

    Writeln('');
    Writeln('Build successful');
    Writeln('Press <Enter> key to quit');
    Readln;

  except
    on E: Exception do begin
      Writeln(E.ClassName, ': ', E.Message);
      Writeln('');
      Writeln('');
      Writeln('Build failed!');
      Writeln('Press <Enter> key to quit');
      Readln;
      halt(1);
    end;
  end;
end.