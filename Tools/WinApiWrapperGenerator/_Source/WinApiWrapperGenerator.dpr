program WinApiWrapperGenerator;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  system.AnsiStrings,
  System.SysUtils,
  WinApi.Windows,
  Alcinoe.files,
  Alcinoe.StringList,
  Alcinoe.StringUtils,
  Alcinoe.common;

{****************************************************************************************}
function RemoveSection(const ACSource: AnsiString; const ADefine: AnsiString): AnsiString;
begin

  //
  // // #if _CREDUI_INFO_DEFINED // ntifs
  // //     _In_opt_ PCREDUI_INFOA pUiInfo,
  // // #else
  // //     _In_opt_ PVOID pUiInfo,
  // // #endif // _CREDUI_INFO_DEFINED
  //

  var LDefine := ADefine; // !_CREDUI_INFO_DEFINED
  var LInverse := ALposA('!', LDefine) = 1;
  if LInverse then delete(LDefine, 1, 1); // _CREDUI_INFO_DEFINED
  var LIgnoreLine := False;

  var LIn := TALStringListA.Create;
  var LOut := TALStringListA.Create;
  try

    LIn.Text := ACSource;
    var LSkipDepth: Integer := 0;

    for var i := 0 to LIn.Count - 1 do begin
      var LLine := LIn[i]; // // #if _CREDUI_INFO_DEFINED // ntifs
      var LTrim := ALTrim(LLine); // // #if _CREDUI_INFO_DEFINED // ntifs
      var P1 := AlposA(' //', LTrim, 4);
      if P1 > 0 then LTrim := ALTrim(AlcopyStr(LTrim, 1, P1-1)); // // #if _CREDUI_INFO_DEFINED

      // Start of a block to skip?
      // // #if _CREDUI_INFO_DEFINED
      if (LSkipDepth = 0) then begin
        if (ALPosIgnoreCaseA(LTrim, '// #if !' + LDefine) = 1) and
           (length(LTrim) >= length('// #if !' + LDefine))  then begin
          if (Not ALSameTextA(LTrim, '// #if !' + LDefine)) then
            Raise Exception.Create('Error BF1D0AD8-3B8F-4EEA-8253-311FB0A61682 - ' + String(LTrim));
          Inc(LSkipDepth);
          LIgnoreLine := LInverse;
          Continue;
        end
        else if (ALPosIgnoreCaseA(LTrim, '// #if ' + LDefine) = 1) and
                (length(LTrim) >= length('// #if ' + LDefine)) then begin
          if (Not ALSameTextA(LTrim, '// #if ' + LDefine)) then
            Raise Exception.Create('Error B9630920-CB07-49CC-BAEC-2ABB105D2F9C - ' + String(LTrim));
          Inc(LSkipDepth);
          LIgnoreLine := not LInverse;
          Continue;
        end;
      end;

      // We are currently skipping a block that started with "#if ... LDefine ..."
      if LSkipDepth > 0 then begin
        // If we hit an #else at the *top* of this skipped block,
        // we stop skipping and keep everything from here on.
        if (LSkipDepth = 1) and
           (ALPosIgnoreCaseA('// #else', LTrim) = 1) then begin
          LIgnoreLine := not LIgnoreLine;
          Continue;
        end;

        // Nested #if inside the skipped block
        if (ALPosIgnoreCaseA('// #if', LTrim) = 1) then begin
          Inc(LSkipDepth);
          If not LIgnoreLine then LOut.Add(LLine);
          Continue;
        end;

        // Matching #endif (possibly for nested levels)
        if (ALPosIgnoreCaseA('// #endif', LTrim) = 1) then begin
          Dec(LSkipDepth);
          If (LSkipDepth > 0) and (not LIgnoreLine) then LOut.Add(LLine);
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

{***********************************************************************************************}
function RemoveSalAnnotation(const ACSource: AnsiString; const ASalName: AnsiString): AnsiString;
begin

  //
  // // _Outptr_ _When_(return != 0, __drv_allocatesMem(Mem)) PSEC_WINNT_AUTH_IDENTITY_OPAQUE* AuthDataCopy
  //

  //
  // // _Outptr_ _When_ (return != 0, __drv_allocatesMem(Mem)) PSEC_WINNT_AUTH_IDENTITY_OPAQUE* AuthDataCopy
  //

  Result := ACSource;
  var P1 := ALPosA(ASalName+'(', Result);
  var P3 := P1 + length(ASalName+'(');
  while P1 > 0 do begin
    if (P1 > low(Result)) and (Result[P1-1] not in [' ', '(', #13, #10]) then begin
      P1 := ALPosA(ASalName+'(', Result, P1+1);
      P3 := P1 + length(ASalName+'(');
      Continue;
    end;
    var P4 := ALPosA('(', Result, P3);
    var P2 := ALPosA(')', Result, P3);
    if P2 <= 0 then raise Exception.Create('Error 00E73C65-6E3A-4383-8869-58A40E3A4894');
    if P4 < P2 then begin
      P3 := P2+1;
      continue;
    end;
    delete(Result, P1, P2-P1+1);
    P1 := ALPosA(ASalName+'(', Result, P1+1);
    P3 := P1 + length(ASalName+'(');
  end;

  P1 := ALPosA(ASalName+' (', Result);
  P3 := P1 + length(ASalName+' (');
  while P1 > 0 do begin
    if (P1 > low(Result)) and (Result[P1-1] not in [' ', '(', #13, #10]) then begin
      P1 := ALPosA(ASalName+' (', Result, P1+1);
      P3 := P1 + length(ASalName+' (');
      Continue;
    end;
    var P4 := ALPosA('(', Result, P3);
    var P2 := ALPosA(')', Result, P3);
    if P2 <= 0 then raise Exception.Create('Error 00E73C65-6E3A-4383-8869-58A40E3A4894');
    if P4 < P2 then begin
      P3 := P2+1;
      continue;
    end;
    delete(Result, P1, P2-P1+1);
    P1 := ALPosA(ASalName+' (', Result, P1+1);
    P3 := P1 + length(ASalName+' (');
  end;

  Result := ALStringReplaceA(Result, ' ' + ASalName+ ' ', ' ');
  Result := ALStringReplaceA(Result, #13#10'// ' + ASalName+ ' ', #13#10'// ');
  Result := ALStringReplaceA(Result, '(' + ASalName+ ' ', '( ');

end;

{************************************************************************************}
function CType2Delphi(const ACtype: AnsiString; const AUseCaret: Boolean): AnsiString;
begin
  Result := ACtype;
  var LPCount: Integer := 0;
  while (Result <> '') and (Result[low(Result)] = '^') do begin
    delete(Result, 1, 1);
    inc(LPCount);
  end;

  if ALSameTextA(Result, 'char') then Result := 'AnsiChar'
  else if ALSameTextA(Result, 'pchar') then Result := 'PAnsiChar'
  else if ALSameTextA(Result, 'float') then Result := 'Single'
  else if ALSameTextA(Result, 'int') then Result := 'Integer'
  else if ALSameTextA(Result, 'long_double') then Result := 'Extended'
  else if ALSameTextA(Result, 'signed_int') then Result := 'Integer'
  else if ALSameTextA(Result, 'signed_short') then Result := 'SHORT'
  else if ALSameTextA(Result, 'unsigned') then Result := 'UINT'
  else if ALSameTextA(Result, 'unsigned_char') then Result := 'UCHAR'
  else if ALSameTextA(Result, 'unsigned_int') then Result := 'UINT'
  else if ALSameTextA(Result, 'unsigned_long') then Result := 'ULONG'
  else if ALSameTextA(Result, 'unsigned_long_long') then Result := 'ULONGLONG'
  else if ALSameTextA(Result, 'unsigned_short') then Result := 'USHORT'
  else if ALSameTextA(Result, 'unsigned_short_int') then Result := 'USHORT';

  for var I := 1 to LPCount do begin
    if AUseCaret then Result := '^' + Result
    else Result := 'P' + Result;
  end;
end;

{******************************************************************}
function CIdentifier2Delphi(const AVarName: AnsiString): AnsiString;
begin
       if ALSameTextA(AVarName, 'array') then Result := '&'+AVarName
  else if ALSameTextA(AVarName, 'destructor') then Result := '&'+AVarName
  else if ALSameTextA(AVarName, 'end') then Result := '&'+AVarName
  else if ALSameTextA(AVarName, 'file') then Result := '&'+AVarName
  else if ALSameTextA(AVarName, 'function') then Result := '&'+AVarName
  else if ALSameTextA(AVarName, 'label') then Result := '&'+AVarName
  else if ALSameTextA(AVarName, 'property') then Result := '&'+AVarName
  else if ALSameTextA(AVarName, 'raise') then Result := '&'+AVarName
  else if ALSameTextA(AVarName, 'set') then Result := '&'+AVarName
  else if ALSameTextA(AVarName, 'string') then Result := '&'+AVarName
  else if ALSameTextA(AVarName, 'type') then Result := '&'+AVarName
  else Result := AVarName;
end;

{*************************************************************}
function ConvertDefine(const ACSource: AnsiString): AnsiString;
begin

  // // #define SECBUFFER_VERSION 0
  // // #define SECBUFFER_EMPTY 0x00000001 // Undefined, replaced by provider

  var LLstSrc := TALStringListA.Create;
  try
    LLstSrc.Text := ACSource;
    for var I := 0 to LLstSrc.Count - 1 do begin
      var LLine := ALTrim(LLstSrc[I]); // // #define SECBUFFER_EMPTY 0x00000001 // Undefined, replaced by provider
      if ALPosA('// #define ', LLine) = 1 then begin
        var J := I + 1;
        While (LLine <> '') and (LLine[high(LLine)] = '\') and (J <= LLstSrc.Count - 1) do begin
          delete(LLine, length(LLine), 1);
          LLine := ALTrim(LLine) + ' ' + ALTrim(AlcopyStr(LLstSrc[J], 4, Maxint));
          LLstSrc[J] := '';
          inc(j);
        end;
        var LComment: AnsiString := '';
        var P1 := AlposA(' //', LLine, 4);
        if P1 <= 0 then P1 := AlposA(' (*', LLine, 4);
        if P1 > 0 then begin
          LComment := ALTrim(AlcopyStr(LLine, P1, ALMaxInt)); // Undefined, replaced by provider
          LLine := ALTrim(AlcopyStr(LLine, 1, P1-1)); // // #define SECBUFFER_EMPTY 0x00000001
        end;
        var LLst := TALStringListA.Create;
        try
          LLst.LineBreak := ' ';
          LLst.Text := LLine; // //
                              // #define
                              // SECBUFFER_EMPTY
                              // 0x00000001
          if LLst.Count >= 4 then begin
            var Lvalue := LLst[3];  // 0x00000001
            for J := 4 to LLst.Count - 1 do
              Lvalue := Lvalue + ' ' + LLst[J];
            P1 := ALPosIgnoreCaseA('(HRESULT)', Lvalue);
            if P1 > 0 then begin
              Lvalue := ALStringReplaceA(Lvalue, '(HRESULT)', '');
              if (Lvalue <> '') and (LValue[low(Lvalue)] <> '(') then Lvalue := '(' + Lvalue + ')';
              Lvalue := ALStringReplaceA(Lvalue, 'L)', ')');
              Lvalue := 'HRESULT' + Lvalue;
              LLst[2] := LLst[2] + ': HRESULT';
            end;
            P1 := ALPosIgnoreCaseA('_HRESULT_TYPEDEF_(', Lvalue);
            if P1 = 1 then begin
              Lvalue := ALStringReplaceA(Lvalue, '_HRESULT_TYPEDEF_(', '(');
              Lvalue := ALStringReplaceA(Lvalue, 'L)', ')');
              Lvalue := 'HRESULT' + Lvalue;
              LLst[2] := LLst[2] + ': HRESULT';
            end;
            P1 := ALPosIgnoreCaseA('(SECURITY_STATUS)', Lvalue);
            if P1 > 0 then begin
              Lvalue := ALStringReplaceA(Lvalue, '(SECURITY_STATUS)', '');
              if (Lvalue <> '') and (LValue[low(Lvalue)] <> '(') then Lvalue := '(' + Lvalue + ')';
              Lvalue := 'SECURITY_STATUS' + Lvalue;
              LLst[2] := LLst[2] + ': SECURITY_STATUS';
            end;
            P1 := ALPosIgnoreCaseA('(ULONGLONG)', Lvalue);
            if P1 > 0 then begin
              Lvalue := ALStringReplaceA(Lvalue, '(ULONGLONG)', '');
              if (Lvalue <> '') and (LValue[low(Lvalue)] <> '(') then Lvalue := '(' + Lvalue + ')';
              Lvalue := 'ULONGLONG' + Lvalue;
              LLst[2] := LLst[2] + ': ULONGLONG';
            end;
            P1 := ALPosIgnoreCaseA('(ULONG)', Lvalue);
            if P1 > 0 then begin
              Lvalue := ALStringReplaceA(Lvalue, '(ULONG)', '');
              if (Lvalue <> '') and (LValue[low(Lvalue)] <> '(') then Lvalue := '(' + Lvalue + ')';
              Lvalue := 'ULONG' + Lvalue;
              LLst[2] := LLst[2] + ': ULONG';
            end;
            P1 := ALPosIgnoreCaseA('(ULONG_PTR)', Lvalue);
            if P1 > 0 then begin
              Lvalue := ALStringReplaceA(Lvalue, '(ULONG_PTR)', '');
              if (Lvalue <> '') and (LValue[low(Lvalue)] <> '(') then Lvalue := '(' + Lvalue + ')';
              Lvalue := 'ULONG_PTR' + Lvalue;
              LLst[2] := LLst[2] + ': ULONG_PTR';
            end;
            P1 := ALPosIgnoreCaseA('ui64', Lvalue);
            if P1 > 0 then begin
              Lvalue := ALStringReplaceA(Lvalue, 'ui64', '');
              if (Lvalue <> '') and (LValue[low(Lvalue)] <> '(') then Lvalue := '(' + Lvalue + ')';
              Lvalue := 'UInt64' + Lvalue;
              LLst[2] := LLst[2] + ': UInt64';
            end;
            if (AlposA('(',LLst[2]) <= 0) and
               ((AlposA('0x',Lvalue) = 1) or
                ((Lvalue <> '') and (Lvalue[low(Lvalue)] = '(') and (Lvalue[high(Lvalue)] = ')')) or
                (AlposA('SECURITY_STATUS(',Lvalue) = 1) or
                (AlposA('HRESULT(',Lvalue) = 1) or
                (AlposA('ULONGLONG(',Lvalue) = 1) or
                (AlposA('ULONG(',Lvalue) = 1) or
                (AlposA('ULONG_PTR(',Lvalue) = 1) or
                (AlposA('UInt64(',Lvalue) = 1) or
                (AlposA('''',Lvalue) = 1) or
                (AlposA('"',Lvalue) = 1) or
                (AlposA('L"',Lvalue) = 1) or
                (AlposA('TEXT("',Lvalue) = 1) or
                ((ALPosA('L', LValue) = length(Lvalue)) and (ALIsNumeric(AlcopyStr(Lvalue, 1, length(LValue)-1)))) or
                (ALIsNumeric(Lvalue)) or
                (alposA('const '+Lvalue+' = ', LLstSrc.Text) > 0)) then begin // const SP_PROT_TLS1_SERVER = $00000040;
                                                                            // const SP_PROT_TLS1_0_SERVER = SP_PROT_TLS1_SERVER;
              if (AlposA('0x',Lvalue) = 1) and (AlposA('L',Lvalue) = Length(Lvalue)) or
                 ((ALPosA('L', LValue) = length(Lvalue)) and (ALIsNumeric(AlcopyStr(Lvalue, 1, length(LValue)-1)))) then
                LValue := ALStringReplaceA(Lvalue, 'L', '', [RfIgnoreCase]); // 0x800000001L => 0x800000001
              LValue := ALStringReplaceA(Lvalue, '0x', '$', [RfIgnoreCase]); // $00000001
              LValue := ALStringReplaceA(Lvalue, ' | ', ' or '); // (HTTP_AUTH_ENABLE_BASIC or HTTP_AUTH_ENABLE_DIGEST)
              IF AlposA('TEXT("',Lvalue) = 1 then begin
                LValue := ALStringReplaceA(Lvalue, 'TEXT("', '"', [RfIgnoreCase]); // TEXT("SslCrackCertificate") => "SslCrackCertificate")
                IF AlposA('")',Lvalue) = length(LValue) - 1 then
                  LValue := ALStringReplaceA(Lvalue, '")', '"', [RfIgnoreCase]) // "SslCrackCertificate"
                else
                  Raise Exception.Create('Error 1F4F6586-3452-486F-AF73-CDEFDD598B47');
              end;
              IF AlposA('L"',Lvalue) = 1 then
                LValue := ALStringReplaceA(Lvalue, 'l"', '"', [RfIgnoreCase]); // $00000001
              LValue := ALStringReplaceA(Lvalue, '"', '''', [rfReplaceALL]); // $00000001
              LLstSrc[I] := alTrim('const ' + LLst[2] + ' = ' + LValue + '; ' + LComment); // const SECBUFFER_EMPTY = $00000001; // Undefined, replaced by provider
            end
            else
              LLstSrc[I] := LLine + ALIfThenA(LComment <> '', ' ' + LComment);
          end;
        finally
          ALFreeAndNil(LLst);
        end;
      end;
    end;
    result := LLstSrc.Text;
  finally
    ALFreeAndNil(LLstSrc);
  end;

end;

{*****************************}
function _ExpandTypedefAliases(
           const ACSource: AnsiString;
           const ABaseCType: AnsiString;
           const AAliases: AnsiString;
           const AComment: AnsiString;
           Const APrefix: AnsiString): AnsiString;
begin

  Result := '';
  var LLst1 := TALStringListA.Create;
  try
    LLst1.LineBreak := ',';
    LLst1.Text := AAliases; // _SECURITY_INTEGER
                            // SECURITY_INTEGER
                            //  * PSECURITY_INTEGER

    var LLst2 := TalStringListA.Create;
    try

      for var I := 0 to LLst1.Count - 1 do begin
        if ALposA(' * * ', LLst1[I]) > 0 then begin // * * PPSECURITY_INTEGER
          var LStr := ALTrim(ALStringReplaceA(LLst1[I],' * * ','',[]));
          Result := Result + APrefix + LStr + ' = ' + CType2Delphi('^P' + ABaseCType, true{AUseCaret}) + ';' + ALIfThenA(AComment <> '', ' ' + AComment) + #13#10;
          LLst2.Add(LStr);
        end
        else if ALposA(' * ', LLst1[I]) > 0 then begin // * PSECURITY_INTEGER
          var LStr := ALTrim(ALStringReplaceA(LLst1[I],' * ','',[]));
          Result := Result + APrefix + LStr + ' = ' + CType2Delphi('^' + ABaseCType, true{AUseCaret}) + ';' + ALIfThenA(AComment <> '', ' ' + AComment) + #13#10;
          LLst2.Add(LStr);
        end
        else begin
          var LStr := alTrim(LLst1[I]);
          Result := Result + APrefix + LStr + ' = ' + CType2Delphi(ABaseCType, true{AUseCaret}) + ';' + ALIfThenA(AComment <> '', ' ' + AComment) + #13#10;
          LLst2.Add(LStr);
        end;
      end;

      for var I := 0 to LLst2.Count - 1 do begin
        if ((AlposA('P' + LLst2[I], ACSource) <= 0) and
            (AlposA('P' + LLst2[I], Result) <= 0) and
            (AlposA(LLst2[I] + ' * ', ACSource) > 0)) then begin
          Result := Result + APrefix + 'P' + LLst2[I] + ' = ^' + LLst2[I] + ';' + ALIfThenA(AComment <> '', ' ' + AComment) + #13#10;
        end;
      end;

    finally
      ALFreeAndNil(LLst2);
    end;

  finally
    ALFreeAndNil(LLst1);
  end;

  Result := ALTrimRight(Result);

end;

{****************************************************************************************************}
function ConvertCParamsToDelphi(const ACParams: AnsiString; Const AIsFromStruct: Boolean): AnsiString;
begin

  var LParamsLst := TALStringListA.Create;
  try
    LParamsLst.Text := ALTrim(ACParams);

    For Var i := LParamsLst.Count - 1 downto 0 do
      if LParamsLst[I] = '' then LParamsLst.Delete(i);
    For Var i := LParamsLst.Count - 1 downto 0 do
      if LParamsLst[I] = '//' then LParamsLst.Delete(i);

    var LCaseOfIndex: Integer := -1;
    var LCaseOfStruct: Boolean := False;

    For Var i := 0 to LParamsLst.Count - 1 do begin
      var LLine := ALTrim(LParamsLst[I]); // _In_opt_ LPSTR pszPrincipal, // Name of principal
      if ALPosA('//', LLine) = 1 then begin
        LParamsLst[I] := LLine;
        if LCaseOfIndex >= 0 then LParamsLst[I] := '  ' + LLine;
        continue;
      end;
      var LComment: AnsiString := '';
      var P1 := AlposA(' //', LLine);
      if P1 <= 0 then P1 := AlposA(' (*', LLine);
      if P1 > 0 then begin
        LComment := ALTrim(AlcopyStr(LLine, P1, ALMaxInt)); // // Name of principal
        LLine := ALTrim(AlcopyStr(LLine, 1, P1-1)); // _In_opt_ LPSTR pszPrincipal,
      end;
      if (LLine <> '') and (LLine[high(LLine)] = ',') then
        LLine := ALTrim(AlCopyStr(LLine, 1, length(LLine) - 1)); // _In_opt_ LPSTR pszPrincipal
      LLine := ' ' + LLine + ' ';
      LLine := ALStringReplaceA(LLine, ' OPTIONAL ', ' ');
      LLine := ALStringReplaceA(LLine, ' IN ', ' ');
      LLine := ALStringReplaceA(LLine, ' OUT ', ' ');
      LLine := ALStringReplaceA(LLine, ' const ', ' ');
      if LCaseOfIndex = -1 then
        LLine := ALStringReplaceA(LLine, ' struct ', ' ');
      LLine := ALStringReplaceA(LLine, ' const ', ' ');
      LLine := ALStringReplaceA(LLine, ';', '', []);
      LLine := ALStringReplaceA(LLine, ' + ', '+', []); // WCHAR Hostname[HTTP_REQUEST_PROPERTY_SNI_HOST_MAX_LENGTH + 1] => WCHAR Hostname[HTTP_REQUEST_PROPERTY_SNI_HOST_MAX_LENGTH+1]
      while alposA('  ', LLine) > 0 do
        LLine := ALStringReplaceA(LLine,'  ', ' ');
      LLine := ALTrim(LLine); // LPSTR pszPrincipal
      if (LLine <> '') and (LLine[high(LLine)]= '*') then LLine := LLine + ' ';
      if (LLine <> '') and (LLine[low(LLine)]= '*') then LLine := ' ' + LLine;
      If (LCaseOfIndex = -1) and (ALSameTextA(LLine, 'union {')) then begin
        LParamsLst[I] := 'case Integer of' + ALIfThenA(LComment <> '', ' ' + LComment);
        LCaseOfIndex := 0;
        Continue;
      end;
      If (LCaseOfIndex <> -1) and (not LCaseOfStruct) and (ALSameTextA(LLine, '}')) then begin
        LParamsLst[I] := '';
        LCaseOfIndex := -1;
        Continue;
      end;
      If (LCaseOfIndex <> -1) and (ALSameTextA(LLine, 'struct {')) then begin
        LParamsLst[I] := '  ' + ALIntToStrA(LCaseOfIndex) + ':(' + ALIfThenA(LComment <> '', ' ' + LComment);
        inc(LCaseOfIndex);
        LCaseOfStruct := True;
        Continue;
      end;
      If (LCaseOfIndex <> -1) and (LCaseOfStruct) and (ALPosA('}', LLine) = 1) then begin
        LCaseOfStruct := False;
        LParamsLst[I] := '    end;'#13#10'  );' + ALIfThenA(LComment <> '', ' ' + LComment);
        delete(LLine,1,1); // } FromMemory => FromMemory
        LLine := ALTrim(LLine); // FromMemory
        if LLine = '' then
          raise Exception.Create('Error 7F4D047D-D19A-40FE-816F-70FDF2F59B01');
        var LRecName := LLine;
        var J := I - 1;
        While J >= 0 do begin
          LLine := ALTrim(LParamsLst[J]);
          if ALPosA(ALIntToStrA(LCaseOfIndex-1) + ':(', LLine) = 1 then begin
            LComment := '';
            P1 := AlposA(' //', LLine);
            if P1 <= 0 then P1 := AlposA(' (*', LLine);
            if P1 > 0 then begin
              LComment := ALTrim(AlcopyStr(LLine, P1, ALMaxInt)); // // Name of principal
              LLine := ALTrim(AlcopyStr(LLine, 1, P1-1)); // _In_opt_ LPSTR pszPrincipal,
            end;
            LParamsLst[J] := '  ' + ALIntToStrA(LCaseOfIndex - 1) + ':('#13#10'    ' + LRecName + ': record' + ALIfThenA(LComment <> '', ' ' + LComment);
            break;
          end;
          dec(J);
        end;
        if J < 0 then raise Exception.Create('Error A4CB5D83-AB0B-41E3-9DBA-456358D82EFA');
        Continue;
      end;
      if ALposA(' * * ', LLine) > 0 then begin
        var LLst := TALStringListA.Create;
        try
          LLst.LineBreak := ' * * ';
          LLst.Text := LLine;
          if LLst.count = 1 then // void * *,
            LLst.Add('param'+ALInttostrA(i));
          if LLst.count <> 2 then
            raise Exception.Create('Error C68C69E1-A865-46D9-A7BD-A588BDDDC704 - ' + String(LLst.Text) + ' - ' + String(LParamsLst.Text));
          LParamsLst[I] := CIdentifier2Delphi(LLst[1]) + ': ' + CType2Delphi('^^' + LLst[0], false{AUseCaret}) + ALIfThenA(AIsFromStruct or (I < LParamsLst.Count - 1), ';') + ALIfThenA(LComment <> '', ' ' + LComment);
        finally
          ALFreeAndNil(LLst);
        end;
      end
      else if ALposA(' * ', LLine) > 0 then begin
        var LLst := TALStringListA.Create;
        try
          LLst.LineBreak := ' * ';
          LLst.Text := LLine;
          if LLst.count = 1 then // void *,
            LLst.Add('param'+ALInttostrA(i));
          if LLst.count <> 2 then
            raise Exception.Create('Error C68C69E1-A865-46D9-A7BD-A588BDDDC704 - ' + String(LLst.Text) + ' - ' + String(LParamsLst.Text));
          LParamsLst[I] := CIdentifier2Delphi(LLst[1]) + ': ' + CType2Delphi('^' + LLst[0], false{AUseCaret}) + ALIfThenA(AIsFromStruct or (I < LParamsLst.Count - 1), ';') + ALIfThenA(LComment <> '', ' ' + LComment);
        finally
          ALFreeAndNil(LLst);
        end;
      end
      else begin
        var LLst := TALStringListA.Create;
        try
          LLst.LineBreak := ' ';
          LLst.Text := LLine; // LPSTR
                              // pszPrincipal
          if (LLst.count = 1) and ALSametextA(LLst[0], 'VOID') then begin
            if LParamsLst.Count <> 1 then raise Exception.Create('Error E117BB4D-8E9C-468E-8A9F-DEE8B8790F62');
            LParamsLst[I] := '';
            continue;
          end;
          if LLst.count = 1 then // unsigned_long,
            LLst.Add('param'+ALInttostrA(i));
          if LLst.count <> 2 then
            raise Exception.Create('Error 512975DE-7E5C-4B37-83E7-AB57DF881629 - ' + String(LLst.Text) + ' - ' + String(LParamsLst.Text));
          if ALPosA('[', LLine) > 0 then begin
            LParamsLst[I] := LLst[1] + ' of ' + CType2Delphi(LLst[0], false{AUseCaret});
            LParamsLst[I] := ALStringReplaceA(LParamsLst[I], '[', ': array[0..');
            LParamsLst[I] := ALStringReplaceA(LParamsLst[I], ']', '-1]');
            LParamsLst[I] := ALStringReplaceA(LParamsLst[I], ']: array[', ', ');
            LParamsLst[I] := LParamsLst[I] + ALIfThenA(AIsFromStruct or (I < LParamsLst.Count - 1), ';') + ALIfThenA(LComment <> '', ' ' + LComment);
          end
          else begin
            LParamsLst[I] := CIdentifier2Delphi(LLst[1]) + ': ' + CType2Delphi(LLst[0], false{AUseCaret}) + ALIfThenA(AIsFromStruct or (I < LParamsLst.Count - 1), ';') + ALIfThenA(LComment <> '', ' ' + LComment);;
          end;
        finally
          ALFreeAndNil(LLst);
        end;
      end;
      LParamsLst[I] := ALTrim(LParamsLst[I]);
      if (LCaseOfIndex >= 0) and (not LCaseOfStruct) then begin
        LLine := LParamsLst[I];
        LComment := '';
        P1 := AlposA(' //', LLine, 4);
        if P1 <= 0 then P1 := AlposA(' (*', LLine, 4);
        if P1 > 0 then begin
          LComment := ALTrim(AlcopyStr(LLine, P1, ALMaxInt)); // // ntifs
          LLine := ALTrim(AlcopyStr(LLine, 1, P1-1)); // // typedef SECURITY_INTEGER * PTimeStamp;
        end;
        LLine := ALStringReplaceA(LLine, ';', '');
        LParamsLst[I] := '  '+ALIntToStrA(LCaseOfIndex) + ':(' + ALTrim(LLine) + ');' + ALIfThenA(LComment <> '', ' ' + LComment);
        inc(LCaseOfIndex);
      end
      else if (LCaseOfIndex >= 0) and (LCaseOfStruct) then begin
        LParamsLst[I] := '      '+ALTrim(LParamsLst[I]);
      end;
    end;

    Result := ALTrim(LParamsLst.Text);

  finally
    ALFReeAndNil(LParamsLst);
  end;

end;

{*****************************************************************}
function ConvertSimpleType(const ACSource: AnsiString): AnsiString;
begin

  //
  // // typedef unsigned_long TimeStamp;
  //

  //
  // // typedef SECURITY_INTEGER * PTimeStamp; // ntifs
  //

  //
  // // typedef LARGE_INTEGER _SECURITY_INTEGER, SECURITY_INTEGER, *PSECURITY_INTEGER; // ntifs
  //

  var LLstSrc := TALStringListA.Create;
  try
    LLstSrc.Text := ACSource;
    for var I := 0 to LLstSrc.Count - 1 do begin
      var LLine := ALTrim(LLstSrc[I]); // // typedef SECURITY_INTEGER * PTimeStamp; // ntifs
      if (ALPosA('// typedef ', LLine) = 1) and
         (ALPosA(' union ', LLine) <= 0) and
         (ALPosA(' struct ', LLine) <= 0) and
         (ALPosA(' enum ', LLine) <= 0) then begin

        var J: Integer := -1;
        if ALPosA(';', LLine) <= 0 then begin
          J := I+1;
          While J <= LLstSrc.Count - 1 do begin
            LLine := LLine + ' ' + AlTrim(AlcopyStr(LLstSrc[J], 4, MaxInt));
            If ALPosA(';', LLine) > 0 then break;
            inc(J);
          end;
          while alposA('  ', LLine) > 0 do
            LLine := ALStringReplaceA(LLine,'  ', ' ');
        end;

        var LComment: AnsiString := '';
        var P1 := AlposA(' //', LLine, 4);
        if P1 <= 0 then P1 := AlposA(' (*', LLine, 4);
        if P1 > 0 then begin
          LComment := ALTrim(AlcopyStr(LLine, P1, ALMaxInt)); // // ntifs
          LLine := ALTrim(AlcopyStr(LLine, 1, P1-1)); // // typedef SECURITY_INTEGER * PTimeStamp;
        end;

        LLine := ALTrim(AlStringReplaceA(LLine,'// typedef CONST ',''));
        LLine := ALTrim(AlStringReplaceA(LLine,'// typedef ','')); // SECURITY_INTEGER * PTimeStamp;
        if AlposA(';', LLine) <= 0 then continue;
        LLine := ALTrim(AlStringReplaceA(LLine,';','')); // SECURITY_INTEGER * PTimeStamp
        if (LLine <> '') and (LLine[high(LLine)]= '*') then LLine := LLine + ' ';
        if (LLine <> '') and (LLine[low(LLine)]= '*') then LLine := ' ' + LLine;
        if AlposA(' ', LLine) <= 0 then continue;
        if AlposA(' (', LLine) > 0 then continue; // typedef BOOL (CALLBACK *PCRYPT_DECRYPT_PRIVATE_KEY_FUNC)(

        var LTypeName: AnsiString;
        var LLst1 := TALStringListA.Create;
        Try
          LLst1.LineBreak := ',';
          LLst1.TrailingLineBreak := False;
          LLst1.Text := LLine; // LARGE_INTEGER _SECURITY_INTEGER
                               // SECURITY_INTEGER
                               //  * PSECURITY_INTEGER
          if LLst1.Count <= 0 then
            raise Exception.Create('Error B12456EA-5B5A-4B14-BA5A-AAB70C12BB52 - ' + String(LLine));
          var LLst2 := TALStringListA.Create;
          try
            LLst2.LineBreak := ' ';
            var LStr := ALTrim(LLst1[0]); // SEC_WCHAR * SECURITY_PSTR
            LStr := ALStringReplaceA(LStr, '* ', '*'); // SEC_WCHAR *SECURITY_PSTR
            LLst2.Text := AlTrim(LStr); // SEC_WCHAR
                                        // *SECURITY_PSTR
            if LLst2.Count <> 2 then
              raise Exception.Create('Error A2F21CB4-9381-4A8A-ACC7-B0455AC09A1D - ' + string(LLst2.Text));
            LTypeName := LLst2[0]; // SEC_WCHAR
            LStr := ALStringReplaceA(LLst2[1], '*', '* '); // * SECURITY_PSTR
            if (LStr <> '') and (LStr[low(LStr)] = '*') then LStr := ' ' + LStr; //  * SECURITY_PSTR
            LLst1[0] := LStr; //  * SECURITY_PSTR
            LLine := LLst1.Text; // _SECURITY_INTEGER, SECURITY_INTEGER, * PSECURITY_INTEGER
          finally
            ALFreeAndNil(LLst2);
          end;
        finally
          AlFreeAndNil(LLst1);
        End;

        LLstSrc[I] := _ExpandTypedefAliases(
                        ACSource, // const ACSource: AnsiString;
                        LTypeName, // const ABaseCType: AnsiString;
                        LLine, // const AAliases: AnsiString;
                        LComment, // const AComment: AnsiString): AnsiString;
                        'type '); // Const APrefix: AnsiString
        for var K := I+1 to J do
          LLstSrc[K] := '';

      end;
    end;

    result := LLstSrc.Text;

  finally
    ALFreeAndNil(LLstSrc);
  end;

end;

{********************************************************************************************}
function _ConvertEnumsOrRecords(const ACSource: AnsiString; Const AEnum: Boolean): AnsiString;
begin

  //
  // // typedef enum _SEC_TRAFFIC_SECRET_TYPE
  // // {
  // //  SecTrafficSecret_None,
  // //  SecTrafficSecret_Client,
  // //  SecTrafficSecret_Server
  // // } SEC_TRAFFIC_SECRET_TYPE, * PSEC_TRAFFIC_SECRET_TYPE;
  //

  //
  // // typedef struct _SecHandle
  // // {
  // //  ULONG_PTR dwLower ;
  // //  ULONG_PTR dwUpper ;
  // // } SecHandle, * PSecHandle ;
  //

  var LTypedefName: AnsiString;
  if AEnum then LTypedefName := 'enum'
  else LTypedefName := 'struct';

  Result := ACSource;
  var P1 := alposA(#13#10'// typedef '+LTypedefName+' ',Result);
  while P1 > 0 do begin
    var LTypeDef := TALStringListA.create;
    try

      var P2: Integer;
      if AEnum then P2 := alposA(#13#10'// }',Result, P1 + 1)
      else begin
        P2 := alposA(#13#10'// }',Result, P1 + 1);
        var P3 := alposA(#13#10'// union {', Result, P1 + 1);
        If (P3 > 0) and (P3 < P2) then begin
          P2 := alposA(#13#10'// };',Result, P3);
          if P2 <= 0 then raise Exception.Create('Error 57D95569-533F-43DB-A4BD-CEE6F6434CB3');
          P2 := alposA(#13#10'// }',Result, P2 + 1);
        end;
      end;
      if P2 <= 0 then raise Exception.Create('Error 0DCAB0CB-D0C3-42E9-9395-1080847A491D');
      LTypeDef.Text := ALTrim(AlCopyStr(Result, P1, P2-P1)); // // typedef enum _SEC_TRAFFIC_SECRET_TYPE
                                                             // // {
                                                             // //  SecTrafficSecret_None,
                                                             // //  SecTrafficSecret_Client,
                                                             // //  SecTrafficSecret_Server
      if LTypeDef.Count < 2 then raise Exception.Create('Error D287A617-DFB9-49A7-99A2-BA881693B157 - ' + string(LTypeDef.Text));
      if LTypeDef[1] <> '// {' then begin // // typedef enum _SECPKG_CRED_CLASS {
                                          // //  SecPkgCredClass_None = 0
        var LFirstLine := ALTrim(LTypeDef[0]); // typedef enum _SECPKG_CRED_CLASS {
        If (LFirstLine <> '') and (LFirstLine[high(LFirstLine)] = '{') then begin
          LTypeDef[0] := ALTrim(ALCopyStr(LFirstLine, 1, length(LFirstLine) - 1)); // // typedef enum _SECPKG_CRED_CLASS
          LTypeDef.Insert(1,'// {'); // // typedef enum _SECPKG_CRED_CLASS
                                     // // {
                                     // //  SecPkgCredClass_None = 0
        end
        else
          raise Exception.Create('Error BCDE5C8F-53CE-4F29-B687-1B3F3D6FF249 - ' + string(LTypeDef.Text));
      end;
      LTypeDef.Delete(1);  // // typedef enum _SEC_TRAFFIC_SECRET_TYPE
                           // //  SecTrafficSecret_None,
                           // //  SecTrafficSecret_Client,
                           // //  SecTrafficSecret_Server

      var LFirstLine := LTypeDef[0]; // // typedef enum _SEC_TRAFFIC_SECRET_TYPE
      LFirstLine := ALStringReplaceA(LFirstLine, '// typedef '+LTypedefName+' ', '', []); // _SEC_TRAFFIC_SECRET_TYPE
      LFirstLine := ALStringReplaceA(LFirstLine, '// typedef '+LTypedefName, '', []); // _SEC_TRAFFIC_SECRET_TYPE
      var LInheritedTypeName: AnsiString := '';
      var LTypeName := AlTrim(LFirstLine); // _SEC_TRAFFIC_SECRET_TYPE
      if LTypeName = '' then begin
        for var I := 1 to maxint do begin
          LTypeName := '_ANON_STRUCT_' + ALIntToStrA(I);
          if ALPosA(LTypeName, Result) <= 0 then break;
        end;
      end;
      var P3 := AlposA(':', LTypeName);
      if P3 > 0 then begin // _HTTP_REQUEST_V2 : _HTTP_REQUEST_V1
        LInheritedTypeName := ALTrim(ALCopyStr(LTypeName, P3 + 1, Maxint));
        if LInheritedTypeName = '' then raise Exception.Create('Error 6FE997A4-5418-422F-BDC1-5ADF61A5A4F9');
        LTypeName := ALTrim(ALCopyStr(LTypeName, 1, P3 - 1));
        if LTypeName = '' then raise Exception.Create('Error 10F99399-D4DE-41FD-8D2C-4B88249CF6A9');
      end;
      if AEnum then LFirstLine := LTypeName + ' = (' // _SEC_TRAFFIC_SECRET_TYPE = (
      else LFirstLine := LTypeName + ' = record'; // _SEC_TRAFFIC_SECRET_TYPE = record
      LFirstLine := '  ' + ALTrim(LFirstLine); //  _SEC_TRAFFIC_SECRET_TYPE = (
      LTypeDef[0] := LFirstLine; //  _SEC_TRAFFIC_SECRET_TYPE = (

      For Var i := 1 to LTypeDef.Count - 1 do
        LTypeDef[I] := ALTrim(ALcopyStr(LTypeDef[I], 4, maxint)); // SecTrafficSecret_None,
                                                                  // SecTrafficSecret_Client,
                                                                  // SecTrafficSecret_Server

      For Var i := LTypeDef.Count - 1 downto 0 do
        if LTypeDef[I] = '' then LTypeDef.Delete(i);
      For Var i := LTypeDef.Count - 1 downto 0 do
        if LTypeDef[I] = '//' then LTypeDef.Delete(i);

      var LLastLine := LTypeDef[LTypeDef.count - 1];
      var LComment: AnsiString := '';
      P3 := ALposA(' //',LLastLine); // SecPkgCredClass_Explicit = 40, // explicitly supplied creds
      if P3 <= 0 then P3 := AlposA(' (*', LLastLine);
      if P3 > 0 then begin
        LComment := ALTrim(AlcopyStr(LLastLine, P3, ALMaxInt)); // explicitly supplied creds
        LLastLine := ALTrimRight(AlcopyStr(LLastLine, 1, P3-1)); // SecPkgCredClass_Explicit = 40,
      end;
      LLastLine := ALStringReplaceA(LLastLine, ',', '',[]); // SecPkgCredClass_Explicit = 40
      if LComment <> '' then LLastLine := LLastLine + ' ' + LComment; // SecPkgCredClass_Explicit = 40 // explicitly supplied creds
      LTypeDef[LTypeDef.count - 1] := LLastLine; // SecPkgCredClass_Explicit = 40 // explicitly supplied creds

      if not AEnum then begin
        if LTypeDef.Count = 0 then raise Exception.Create('Error 69AA76EB-8AF2-4446-8B08-F13B554D522E');
        LFirstLine := LTypeDef[0];
        LTypeDef.Delete(0);
        LTypeDef.Text := ConvertCParamsToDelphi(LTypeDef.Text, true{AIsFromStruct});
        LTypeDef.Insert(0, LFirstLine);
        if LInheritedTypeName <> '' then begin
          P3 := ALposA(#13#10 + '  ' + LInheritedTypeName + ' = record'#13#10, Result);
          If P3 <= 0 then raise Exception.Create('Error A7921A48-B03E-461F-A8C6-9950D125C165');
          inc(P3, length(#13#10 + '  ' + LInheritedTypeName + ' = record'#13#10));
          var P4 := ALposA(#13#10'  end;'#13#10, Result, P3);
          if P4 <= 0 then raise Exception.Create('Error 232F302B-A100-461E-B58E-925B15431260');
          var LinheritedFields := AlCopyStr(Result, P3, P4-P3);
          var LLst := TAlStringListA.Create;
          try
            LLst.Text := LinheritedFields;
            For var I := LLst.Count - 1 downto 0 do
              LTypeDef.Insert(1, ALStringReplaceA(LLst[i], '    ', '', []));
          finally
            ALFreeAndNil(LLst);
          end;
        end;
      end
      else begin
        if LInheritedTypeName <> '' then raise Exception.Create('Error 7E4868A4-5602-4ECD-AFFB-52011C306044');
        LTypeDef.Text := ALStringReplaceA(LTypeDef.Text, '= 0x', '= $'); // SCH_EXTENSIONS_OPTIONS_NONE = 0x0, => SCH_EXTENSIONS_OPTIONS_NONE = $0,
      end;

      For Var i := LTypeDef.Count - 1 downto 1 do
        LTypeDef[I] := '    ' + LTypeDef[I]; //     SecTrafficSecret_None,
                                             //     SecTrafficSecret_Client,
                                             //     SecTrafficSecret_Server

      LTypeDef.Insert(0, 'type');  // type
                                   //   _SEC_TRAFFIC_SECRET_TYPE = (
                                   //       SecTrafficSecret_None,
                                   //       SecTrafficSecret_Client,
                                   //       SecTrafficSecret_Server);

      delete(Result, P1, P2-P1);
      P3 := ALPosA(';', Result, P1);
      if P3 <= 0 then raise Exception.Create('Error 315D0F5D-6F8E-4EAB-8EA8-5DF01C980F91');
      inc(P3);
      var LStr := ALCopyStr(Result,P1,P3-P1); // } SEC_TRAFFIC_SECRET_TYPE, * PSEC_TRAFFIC_SECRET_TYPE
      if alposA(#13#10'// }', LStr) <> 1 then Raise Exception.Create('Error CEB13636-81F2-4AF8-9044-90D1E291EDA0 - ' + String(LStr));
      delete(LStr, 1, 6); // SEC_TRAFFIC_SECRET_TYPE, * PSEC_TRAFFIC_SECRET_TYPE;
      LStr := ALStringReplaceA(LStr, #13#10'// ', ' ');
      if alposA('//', LStr) > 0 then
        Raise Exception.Create('Error D338B91D-505E-4CA3-B843-C8711C2CC670 - ' + String(LStr));
      LStr := ALStringReplaceA(LStr, ';', '', []);
      if (LStr <> '') and (LStr[high(LStr)]= '*') then LStr := LStr + ' ';
      if (LStr <> '') and (LStr[low(LStr)]= '*') then LStr := ' ' + LStr;
      LStr := _ExpandTypedefAliases(
                ACSource, // const ACSource: AnsiString;
                LTypeName, // const ABaseCType: AnsiString;
                LStr, // const AAliases: AnsiString;
                '', // const AComment: AnsiString): AnsiString;
                '  '); // Const APrefix: AnsiString

      var LTypeDefStr := ALTrimRight(LTypeDef.Text) + #13#10; // type
                                                              //   _SEC_TRAFFIC_SECRET_TYPE = (
                                                              //       SecTrafficSecret_None,
                                                              //       SecTrafficSecret_Client,
                                                              //       SecTrafficSecret_Server
      if AEnum then LTypeDefStr := LTypeDefStr + '  );' + #13#10 // type
                                                                 //   _SEC_TRAFFIC_SECRET_TYPE = (
                                                                 //       SecTrafficSecret_None,
                                                                 //       SecTrafficSecret_Client,
                                                                 //       SecTrafficSecret_Server
                                                                 //   );
      else LTypeDefStr := LTypeDefStr + '  end;' + #13#10;  // type
                                                            //   _SEC_TRAFFIC_SECRET_TYPE = record
                                                            //       SecTrafficSecret_None,
                                                            //       SecTrafficSecret_Client,
                                                            //       SecTrafficSecret_Server
                                                            //   end;
      LTypeDefStr := LTypeDefStr + LStr; // type
                                         //   _SEC_TRAFFIC_SECRET_TYPE = (
                                         //       SecTrafficSecret_None,
                                         //       SecTrafficSecret_Client,
                                         //       SecTrafficSecret_Server
                                         //   );
                                         //   SEC_TRAFFIC_SECRET_TYPE = _SEC_TRAFFIC_SECRET_TYPE;
                                         //   PSEC_TRAFFIC_SECRET_TYPE = ^_SEC_TRAFFIC_SECRET_TYPE;

      delete(Result,P1, P3-P1);
      insert(#13#10+LTypeDefStr,Result,P1);

      P1 := alposA(#13#10'// typedef '+LTypedefName+' ',Result, P1+1);

    finally
      ALFreeAndNil(LTypeDef);
    end;
  end;

end;

{************************************************************}
function ConvertEnums(const ACSource: AnsiString): AnsiString;
begin
  Result := _ConvertEnumsOrRecords(ACSource, true{AEnum});
end;

{**************************************************************}
function ConvertRecords(const ACSource: AnsiString): AnsiString;
begin
  Result := _ConvertEnumsOrRecords(ACSource, false{AEnum});
end;

{*************************************************************}
function GetDllImport(const AProcName: AnsiString): Ansistring;
const
  CandidateDlls: array[0..5] of PChar = (
    'httpapi.dll',
    'advapi32.dll',
    'crypt32.dll',
    'secur32.dll',
    'schannel.dll',
    'sspicli.dll');
begin
  for var LDll in CandidateDlls do begin
    var LMod := LoadLibraryExW(LDll, 0, $00000800{LOAD_LIBRARY_SEARCH_SYSTEM32});
    if LMod = 0 then Continue;
    try
      if GetProcAddress(LMod, PAnsiChar(AProcName)) <> nil then begin
        Result := ansiString(LDll);
        Exit;
      end;
    finally
      FreeLibrary(LMod);
    end;
  end;
  Result := '';
end;

{****************************************************************}
function ConvertFunctions(const ACSource: AnsiString): AnsiString;
begin

  //
  // // SECURITY_STATUS WINAPI
  // // AcquireCredentialsHandleA(
  // //     _In_opt_  LPSTR pszPrincipal,                 // Name of principal
  // //     _In_      LPSTR pszPackage,                   // Name of package
  // //     _In_      unsigned long fCredentialUse,       // Flags indicating use
  // //     _In_opt_  void * pvLogonId,                   // Pointer to logon ID
  // //     _In_opt_  void * pAuthData,                   // Package specific data
  // //     _In_opt_  SEC_GET_KEY_FN pGetKeyFn,           // Pointer to GetKey() func
  // //     _In_opt_  void * pvGetKeyArgument,            // Value to pass to GetKey()
  // //     _Out_     PCredHandle phCredential,           // (out) Cred Handle
  // //     _Out_opt_ PTimeStamp ptsExpiry                // (out) Lifetime (optional)
  // //     );
  //

  //
  // // typedef SECURITY_STATUS
  // // (WINAPI * ENCRYPT_MESSAGE_FN)(
  // //     PCtxtHandle, unsigned long, PSecBufferDesc, unsigned long);
  //

  Result := ACSource;

  var P1 := alposA('WINAPI',Result);
  while P1 > 0 do begin
    if (P1 <= low(Result)) or
       (Result[P1-1] not in [#13,#10, ' ', '(']) or
       (P1 + length('WINAPI') > high(Result)) or
       (Result[P1+ length('WINAPI')] not in [#13,#10, ' ']) then begin
      P1 := alposA('WINAPI',Result, P1+1);
      continue;
    end;
    var P2 := P1;
    while (P2 > 0) and (alposA(#13#10'// '#13#10, Result, P2) <> P2) and (Result[P2] not in ['"', '.', ';', '-']) do dec(P2);
    if P2 = 0 then raise Exception.Create('Error 5C59AC01-3BB5-446B-BDE1-2E143B187E23');
    if Result[P2] in ['"', '.', ';', '-'] then inc(P2)
    else inc(P2, length(#13#10'// '#13#10));
    var P3 := P1;
    while (P3 <= high(Result)) and (Result[P3] <> '(') do inc(P3);
    if P3 > high(Result) then raise Exception.Create('Error C2DDA7D8-5128-44D5-BFE7-1A2971A73177');

    var LRoutinePrefix: AnsiString := '';
    var LRoutineSuffix: AnsiString := '';
    var LRoutineName: AnsiString := '';
    var LDllImport: AnsiString := '';
    var LHeaderStr := AlCopyStr(Result, P2, P3-P2);  // // SECURITY_STATUS WINAPI
                                                     // // AcquireCredentialsHandleA
    LHeaderStr := ALStringReplaceA(LHeaderStr, '(WINAPI * ', ''); // // typedef SECURITY_STATUS
                                                                  // // ENCRYPT_MESSAGE_FN)(
    LHeaderStr := ALStringReplaceA(LHeaderStr, 'WINAPI', ''); // // SECURITY_STATUS
                                                              // // AcquireCredentialsHandleA
    var LIsCallbackFunctionPointer := ALPosA('typedef', LHeaderStr) > 0;
    LHeaderStr := ALStringReplaceA(LHeaderStr, 'typedef', ''); // // SECURITY_STATUS
                                                               // // ENCRYPT_MESSAGE_FN)
    LHeaderStr := ALStringReplaceA(LHeaderStr, ')', ''); // // SECURITY_STATUS
                                                         // // ENCRYPT_MESSAGE_FN
    var LHeaderLst := TALStringListA.Create;
    try
      LHeaderLst.Text := LHeaderStr; // // SECURITY_STATUS
                                     // // AcquireCredentialsHandleA
      For Var i := 0 to LHeaderLst.Count - 1 do
        LHeaderLst[I] := ALTrim(ALcopyStr(LHeaderLst[I], 4, maxint)); // SECURITY_STATUS
                                                                      // AcquireCredentialsHandleA
      For Var i := LHeaderLst.Count - 1 downto 0 do
        if LHeaderLst[I] = '' then LHeaderLst.Delete(i);
      For Var i := LHeaderLst.Count - 1 downto 0 do
        if AlposA('//', LHeaderLst[I]) = 1 then LHeaderLst.Delete(i);

      if (LHeaderLst.Count <> 2) then
        raise Exception.Create('Error 30F920B9-FDF2-4483-842A-2C86C4E887AC - ' + string(LHeaderStr));

      if ALSameTextA(ALTrim(LHeaderLst[0]), 'VOID') then LRoutinePrefix := 'procedure '
      else begin
        LRoutinePrefix := 'function ';
        LRoutineSuffix := ': ' + CType2Delphi(LHeaderLst[0], false{AUseCaret}); // : SECURITY_STATUS
      end;
      LRoutineName := ALTrim(LHeaderLst[1]);
      LDllImport := GetDllImport(LRoutineName);
      if (LDllImport = '') and (not LIsCallbackFunctionPointer) then begin
        P1 := alposA('WINAPI',Result, P1+1);
        continue;
      end;
      if LIsCallbackFunctionPointer then begin
        LRoutinePrefix := 'type' + #13#10 +
                        '  ' + LRoutineName + ' = ' + LRoutinePrefix; // Type
                                                                      //   ENCRYPT_MESSAGE_FN = function(
      end
      else begin
        LRoutinePrefix := LRoutinePrefix + LRoutineName; // function SecAllocateAndSetCallTarget
      end;
    finally
      ALFreeAndNil(LHeaderLst);
    end;

    inc(P3);
    var P4 := alposA(');', Result, P3);
    if P4 <= 0 then raise Exception.Create('Error 24A828D7-A5D4-483C-95FA-88D15A5535EC');
    var LParamsStr := AlCopyStr(Result, P3, P4-P3); // // _In_opt_ LPSTR pszPrincipal, // Name of principal
                                                    // // _In_ LPSTR pszPackage, // Name of package
                                                    // // _Out_opt_ PTimeStamp ptsExpiry // (out) Lifetime (optional)
                                                    // //
    if AlposA(' //', LParamsStr) <= 0 then
      LParamsStr := ALStringReplaceA(LParamsStr, ',', ','#13#10'// '); // // PCtxtHandle, unsigned_long, PSecBufferDesc
                                                                       // =>
                                                                       // // PCtxtHandle,
                                                                       // // unsigned_long,
                                                                       // // PSecBufferDesc
    var LParamsLst := TALStringListA.Create;
    try
      LParamsLst.Text := LParamsStr;
      For Var i := 1 to LParamsLst.Count - 1 do
        LParamsLst[I] := ALTrim(ALcopyStr(LParamsLst[I], 4, maxint)); // _In_opt_  LPSTR pszPrincipal, // Name of principal
                                                                      // _In_      LPSTR pszPackage, // Name of package
                                                                      // _Out_opt_ PTimeStamp ptsExpiry // (out) Lifetime (optional)
                                                                      //

      if LParamsLst.Count = 0 then raise Exception.Create('Error 69AA76EB-8AF2-4446-8B08-F13B554D522E');
      LParamsLst.Text := ConvertCParamsToDelphi(LParamsLst.Text, False{AIsFromStruct});

      For Var i := LParamsLst.Count - 1 downto 1 do
        LParamsLst[I] := '  ' + ALIfThenA(LIsCallbackFunctionPointer, '  ') + ALTrim(LParamsLst[I]);

      var LRoutineDefinition := aLTrim(LParamsLst.Text);
      LRoutineDefinition := LRoutinePrefix +
                            ALIfThenA(
                              LRoutineDefinition <> '',
                              '('#13#10'  ' + ALIfThenA(LIsCallbackFunctionPointer, '  ') + ALTrim(LParamsLst.Text) + #13#10+ ALIfThenA(LIsCallbackFunctionPointer, '  ') +')',
                              '()') +
                            LRoutineSuffix +
                            '; stdcall;'+ALIfThenA(not LIsCallbackFunctionPointer, ' external '''+LDllImport+''';');

      delete(Result,P2, P4-P2+2{);});
      insert(#13#10+LRoutineDefinition,Result,P2);

    finally
      ALFReeAndNil(LParamsLst);
    end;

    P1 := alposA('WINAPI',Result, P2);
  end;

end;

begin
  try

    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    Writeln;
    Writeln('Which header/unit do you want to convert?');
    Writeln('  1) sspi.h     -> Alcinoe.WinApi.SSPI.pas');
    Writeln('  2) schannel.h -> Alcinoe.WinApi.SChannel.pas');
    Writeln('  3) http.h     -> Alcinoe.WinApi.Http.pas');
    Writeln('  4) winerror.h -> Alcinoe.WinApi.WinError.h');
    Writeln;
    Write('Choice [1-4, empty = all]: ');

    var LChoiceStr: string;
    Readln(LChoiceStr);
    Var LChoices: TArray<Integer>;
    var LInt: integer;
    if ALTryStrToInt(LChoiceStr, LInt) then LChoices := [LInt]
    else LChoices := [1,2,3,4];

    for var LChoice in LChoices do begin

      var LSrcfileName: String;
      var LDstfileName: String;
      var LExtraUses: AnsiString;
      var LExtraTypes: AnsiString;

      {$REGION 'sspi.h'}
      if LChoice = 1 then begin
        Writeln('Converting sspi.h -> Alcinoe.WinApi.SSPI.pas');
        LSrcfileName := ALGetModulePathW + 'sspi.h';
        LDstfileName := ALGetModulePathW + '..\..\Source\Alcinoe.WinApi.SSPI.pas';
        LExtraUses := '''
          Winapi.WinCred,
          Alcinoe.WinApi.Windows;
        ''';
        LExtraTypes := '';
      end;
      {$ENDREGION}

      {$REGION 'schannel.h'}
      if LChoice = 2 then begin
        Writeln('Converting schannel.h -> Alcinoe.WinApi.SChannel.pas');
        LSrcfileName := ALGetModulePathW + 'schannel.h';
        LDstfileName := ALGetModulePathW + '..\..\Source\Alcinoe.WinApi.SChannel.pas';
        LExtraUses := '''
          Alcinoe.WinApi.SSPI,
          Alcinoe.WinApi.Windows;
        ''';
        LExtraTypes := '''
        type
          ALG_ID = UINT;
          PALG_ID = ^ALG_ID;
          PPCCERT_CONTEXT = ^PCCERT_CONTEXT;
          HCRYPTPROV = ULONG_PTR;
        ''';
      end;
      {$ENDREGION}

      {$REGION 'http.h'}
      if LChoice = 3 then begin
        Writeln('Converting http.h -> Alcinoe.WinApi.Http.pas');
        LSrcfileName := ALGetModulePathW + 'http.h';
        LDstfileName := ALGetModulePathW + '..\..\Source\Alcinoe.WinApi.Http.pas';
        LExtraUses := '''
          Winapi.Winsock2,
          Alcinoe.WinApi.Windows;
        ''';
        LExtraTypes := '';
      end;
      {$ENDREGION}

      {$REGION 'winerror.h'}
      if LChoice = 4 then begin
        Writeln('Converting winerror.h -> Alcinoe.WinApi.WinError.pas');
        LSrcfileName := ALGetModulePathW + 'winerror.h';
        LDstfileName := ALGetModulePathW + '..\..\Source\Alcinoe.WinApi.WinError.pas';
        LExtraUses := '';
        LExtraTypes := '';
      end;
      {$ENDREGION}

      {$REGION 'wincrypt.h'}
      //LSrcfileName := ALGetModulePathW + 'wincrypt.h';
      //LDstfileName := ALGetModulePathW + '..\..\Source\Alcinoe.WinApi.WinCrypt.pas';
      //LExtraUses := '';
      //LExtraTypes := '';
      {$ENDREGION}

      var LSrc := ALGetStringFromFile(LSrcfileName);

      LSrc := ALStringReplaceA(LSrc, #9, ' ');
      LSrc := ALStringReplaceA(LSrc, #13, '');
      LSrc := ALStringReplaceA(LSrc, #10, #13#10);
      if alposA(#1, LSrc) > 0 then raise Exception.Create('Error 84E23D4F-94B3-4540-923C-6CA3F200F578');
      if alposA(#2, LSrc) > 0 then raise Exception.Create('Error 84E23D4F-94B3-4540-923C-6CA3F200F578');
      LSrc := ALStringReplaceA(LSrc, '/*', #1);
      LSrc := ALStringReplaceA(LSrc, '*/', #2);
      LSrc := ALStringReplaceA(LSrc, '*', ' * ');
      LSrc := ALStringReplaceA(LSrc, '//', ' // ');
      while alposA(' '#13#10, LSrc) > 0 do
        LSrc := ALStringReplaceA(LSrc,' '#13#10, #13#10);
      while alposA('  ', LSrc) > 0 do
        LSrc := ALStringReplaceA(LSrc,'  ', ' ');
      while alposA('# ', LSrc) > 0 do
        LSrc := ALStringReplaceA(LSrc,'# ', '#');  // #  define  =>  #define
                                                   // #  ifdef WIN32_CHICAGO  =>  #ifdef WIN32_CHICAGO
                                                   // #  elif  =>  #elif
      LSrc := ALStringReplaceA(LSrc, ' [', '[');
      LSrc := ALStringReplaceA(LSrc, #1, '(*');
      LSrc := ALStringReplaceA(LSrc, #2, '*)');

      LSrc := ALStringReplaceA(LSrc,'#ifndef ', '#if !');
      LSrc := ALStringReplaceA(LSrc,'#ifdef ', '#if ');
      LSrc := ALStringReplaceA(LSrc,'#elif ', '#endif'#13#10'#if ');

      LSrc := ALStringReplaceA(LSrc, 'signed int', 'signed_int');
      LSrc := ALStringReplaceA(LSrc, 'signed short', 'signed_short');
      LSrc := ALStringReplaceA(LSrc, 'unsigned char', 'unsigned_char');
      LSrc := ALStringReplaceA(LSrc, 'unsigned short', 'unsigned_short');
      LSrc := ALStringReplaceA(LSrc, 'unsigned short int', 'unsigned_short_int');
      LSrc := ALStringReplaceA(LSrc, 'unsigned int', 'unsigned_int');
      LSrc := ALStringReplaceA(LSrc, 'unsigned long long','unsigned_long_long');
      LSrc := ALStringReplaceA(LSrc, 'unsigned long', 'unsigned_long');
      LSrc := ALStringReplaceA(LSrc, 'long double', 'long_double');

      LSrc := ALStringReplaceA(LSrc,#13#10'_Check_return_'#13#10, #13#10);
      LSrc := ALStringReplaceA(LSrc,#13#10'_Must_inspect_result_'#13#10, #13#10);
      LSrc := ALStringReplaceA(LSrc,#13#10'EXTERN_C'#13#10, #13#10);
      LSrc := ALStringReplaceA(LSrc,#13#10' union'#13#10' {', #13#10' union {');
      LSrc := ALStringReplaceA(LSrc,#13#10' struct'#13#10' {', #13#10' struct {');
      LSrc := ALStringReplaceA(LSrc,#13#10'extern "C" {'#13#10, #13#10);
      LSrc := ALStringReplaceA(LSrc,#13#10'} // extern "C"'#13#10, #13#10);

      LSrc := '//' + ALStringReplaceA(#13#10 + LSrc, #13#10, #13#10'// ');

      while alposA('  ', LSrc) > 0 do
        LSrc := ALStringReplaceA(LSrc,'  ', ' ');

      LSrc := RemoveSalAnnotation(LSrc, '_Field_size_bytes_');
      LSrc := RemoveSalAnnotation(LSrc, '_Field_size_');
      LSrc := RemoveSalAnnotation(LSrc, '_In_reads_bytes_opt_');
      LSrc := RemoveSalAnnotation(LSrc, '_In_reads_bytes_');
      LSrc := RemoveSalAnnotation(LSrc, '_In_reads_opt_');
      LSrc := RemoveSalAnnotation(LSrc, '_In_reads_');
      LSrc := RemoveSalAnnotation(LSrc, '_Outptr_opt_result_maybenull_');
      LSrc := RemoveSalAnnotation(LSrc, '_Outptr_result_bytebuffer_');
      LSrc := RemoveSalAnnotation(LSrc, '_Outptr_result_maybenull_');
      LSrc := RemoveSalAnnotation(LSrc, '_Outptr_');
      LSrc := RemoveSalAnnotation(LSrc, '_Out_writes_bytes_to_opt_');
      LSrc := RemoveSalAnnotation(LSrc, '_Out_writes_bytes_to_');
      LSrc := RemoveSalAnnotation(LSrc, '_Out_writes_to_opt_');
      LSrc := RemoveSalAnnotation(LSrc, '_Out_writes_bytes_');
      LSrc := RemoveSalAnnotation(LSrc, '_Out_writes_bytes_opt_');
      LSrc := RemoveSalAnnotation(LSrc, '_Out_opt_');
      LSrc := RemoveSalAnnotation(LSrc, '_Out_');
      LSrc := RemoveSalAnnotation(LSrc, '_Inout_updates_bytes_to_opt_');
      LSrc := RemoveSalAnnotation(LSrc, '_Inout_updates_bytes_to_');
      LSrc := RemoveSalAnnotation(LSrc, '_Inout_updates_bytes_');
      LSrc := RemoveSalAnnotation(LSrc, '_Inout_updates_');
      LSrc := RemoveSalAnnotation(LSrc, '_Inout_opt_');
      LSrc := RemoveSalAnnotation(LSrc, '_Inout_');
      LSrc := RemoveSalAnnotation(LSrc, '_In_opt_');
      LSrc := RemoveSalAnnotation(LSrc, '_In_');
      LSrc := RemoveSalAnnotation(LSrc, '_Return_type_success_');
      LSrc := RemoveSalAnnotation(LSrc, '_Field_range_');
      LSrc := RemoveSalAnnotation(LSrc, '_Success_');
      LSrc := RemoveSalAnnotation(LSrc, '_Reserved_');
      LSrc := RemoveSalAnnotation(LSrc, '_Post_');
      LSrc := RemoveSalAnnotation(LSrc, '_NullNull_terminated_');
      LSrc := RemoveSalAnnotation(LSrc, '_When_');
      LSrc := RemoveSalAnnotation(LSrc, '_At_');
      LSrc := RemoveSalAnnotation(LSrc, '__bcount');
      LSrc := RemoveSalAnnotation(LSrc, '__in');
      LSrc := RemoveSalAnnotation(LSrc, '__out');
      LSrc := RemoveSalAnnotation(LSrc, '__drv_freesMem');
      LSrc := RemoveSalAnnotation(LSrc, '__callback');

      LSrc := RemoveSection(LSrc, '!__cplusplus');
      LSrc := RemoveSection(LSrc, '!defined(__cplusplus) && !defined(SORTPP_PASS)');
      LSrc := RemoveSection(LSrc, '!defined (_MSC_VER)');
      LSrc := RemoveSection(LSrc, '!( _MSC_VER >= 800 )');
      LSrc := RemoveSection(LSrc, '!_MSC_VER > 1000');
      LSrc := RemoveSection(LSrc, '!(_MSC_VER > 1020)');
      LSrc := RemoveSection(LSrc, '!_MSC_VER >= 1200');
      LSrc := RemoveSection(LSrc, '!UNICODE');
      LSrc := RemoveSection(LSrc, '!WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)');
      LSrc := RemoveSection(LSrc, '!WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP | WINAPI_PARTITION_PHONE_RESTRICTED | WINAPI_PARTITION_GAMES)');
      LSrc := RemoveSection(LSrc, '!WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP | WINAPI_PARTITION_SYSTEM | WINAPI_PARTITION_GAMES)');
      LSrc := RemoveSection(LSrc, '!WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP|WINAPI_PARTITION_PHONE_RESTRICTED | WINAPI_PARTITION_SYSTEM | WINAPI_PARTITION_GAMES)');
      LSrc := RemoveSection(LSrc, '!WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP | WINAPI_PARTITION_SYSTEM)');
      LSrc := RemoveSection(LSrc, '!NTDDI_VERSION > NTDDI_WS03SP1');
      LSrc := RemoveSection(LSrc, '!NTDDI_VERSION > NTDDI_WS03');
      LSrc := RemoveSection(LSrc, '!NTDDI_VERSION > NTDDI_WINBLUE');
      LSrc := RemoveSection(LSrc, '!(NTDDI_VERSION >= NTDDI_WIN10_19H1)');
      LSrc := RemoveSection(LSrc, '!(OSVER(NTDDI_VERSION) > NTDDI_WIN2K)');
      LSrc := RemoveSection(LSrc, '!OSVER(NTDDI_VERSION) > NTDDI_WIN2K');
      LSrc := RemoveSection(LSrc, '!(NTDDI_VERSION >= NTDDI_WIN7)');
      LSrc := RemoveSection(LSrc, '!(NTDDI_VERSION >= NTDDI_WIN8)');
      LSrc := RemoveSection(LSrc, '!(NTDDI_VERSION >= NTDDI_VISTA)');
      LSrc := RemoveSection(LSrc, '!(NTDDI_VERSION >= NTDDI_WINXP)');
      LSrc := RemoveSection(LSrc, '!(NTDDI_VERSION > NTDDI_WINXPSP2)');
      LSrc := RemoveSection(LSrc, '!(NTDDI_VERSION >= NTDDI_WIN10_RS1)');
      LSrc := RemoveSection(LSrc, '!(NTDDI_VERSION >= NTDDI_WS03)');
      LSrc := RemoveSection(LSrc, '!_WIN32_WINNT >= 0x0600');
      LSrc := RemoveSection(LSrc, '!_WIN32_WINNT >= _WIN32_WINNT_WIN7');
      LSrc := RemoveSection(LSrc, '!_WIN32_WINNT >= _WIN32_WINNT_WIN8');
      LSrc := RemoveSection(LSrc, '!_WIN32_WINNT >= _WIN32_WINNT_WIN10');
      LSrc := RemoveSection(LSrc, '!_WIN32_WINNT >= 0x0501');
      LSrc := RemoveSection(LSrc, '!defined(_WIN32) && !defined(_MAC)');
      LSrc := RemoveSection(LSrc, '_WIN32_WINNT < 0x0600');

      {$REGION 'sspi.h'}
      if LChoice = 1 then begin
        LSrc := RemoveSection(LSrc, '__SSPI_H__');
        LSrc := RemoveSection(LSrc, 'defined(_NO_KSECDD_IMPORT_)');
        LSrc := RemoveSection(LSrc, 'WIN32_CHICAGO');
        LSrc := RemoveSection(LSrc, '__SECHANDLE_DEFINED__');
        LSrc := RemoveSection(LSrc, '__SECSTATUS_DEFINED__');
        LSrc := RemoveSection(LSrc, '!defined(_NTDEF_) || defined(_WINNT_)');
        LSrc := RemoveSection(LSrc, '_NTDEF_');
        LSrc := RemoveSection(LSrc, 'MIDL_PASS');
        LSrc := RemoveSection(LSrc, '!ISSP_LEVEL');
        LSrc := RemoveSection(LSrc, 'ISSP_MODE == 0');
        LSrc := RemoveSection(LSrc, '!ISSP_MODE != 0');
        LSrc := RemoveSection(LSrc, '!SECURITY_WIN32');
        LSrc := RemoveSection(LSrc, 'SECURITY_MAC');
        LSrc := RemoveSection(LSrc, 'SECURITY_KERNEL');
        LSrc := RemoveSection(LSrc, 'SECURITY_DOS');
        LSrc := RemoveSection(LSrc, '_AUTH_IDENTITY_DEFINED');
        LSrc := RemoveSection(LSrc, '_AUTH_IDENTITY_EX2_DEFINED');
        LSrc := RemoveSection(LSrc, '_AUTH_IDENTITY_INFO_DEFINED');
        LSrc := RemoveSection(LSrc, 'SEC_WINNT_AUTH_IDENTITY_VERSION');
        LSrc := RemoveSection(LSrc, '_SSPIPFC_NONE_');
        LSrc := RemoveSection(LSrc, '!_CREDUI_INFO_DEFINED');
        LSrc := RemoveSection(LSrc, '!_SEC_WINNT_AUTH_TYPES');

        LSrc := ALStringReplaceA(LSrc,'SEC_TEXT("InitSecurityInterfaceW")','"InitSecurityInterfaceW"');
        LSrc := ALStringReplaceA(LSrc,'SEC_TEXT("InitSecurityInterfaceA")','"InitSecurityInterfaceA"');
        LSrc := ALStringReplaceA(LSrc,'#define KSECDDDECLSPEC __declspec(dllimport)','');
        LSrc := ALStringReplaceA(LSrc,#13#10'// KSECDDDECLSPEC'#13#10,#13#10);
        LSrc := ALStringReplaceA(LSrc,'#define SEC_TEXT TEXT','');
        LSrc := ALStringReplaceA(LSrc,'#define SEC_FAR','');
        LSrc := ALStringReplaceA(LSrc,'#define SEC_ENTRY __stdcall','');
        LSrc := ALStringReplaceA(LSrc,' SEC_ENTRY'#13#10,' WINAPI'#13#10);
        LSrc := ALStringReplaceA(LSrc,'(SEC_ENTRY * ','(WINAPI * ');
        LSrc := ALStringReplaceA(LSrc,'#define __SEC_FAR SEC_FAR','');
        LSrc := ALStringReplaceA(LSrc,' SEC_FAR * ',' * ');
      end;
      {$ENDREGION}

      {$REGION 'schannel.h'}
      if LChoice = 2 then begin
        LSrc := RemoveSection(LSrc, '__SCHANNEL_H__');
        LSrc := RemoveSection(LSrc, '!SCHANNEL_USE_BLACKLISTS');

        LSrc := ALStringReplaceA(
                  LSrc,
                  '// struct _HMAPPER;',
                  '// typedef struct _HMAPPER'#13#10+
                  '// {'#13#10+
                  '// } * P_HMAPPER, * * PP_HMAPPER;', []);
      end;
      {$ENDREGION}

      {$REGION 'http.h'}
      if LChoice = 3 then begin
        LSrc := RemoveSection(LSrc, '__HTTP_H__');
        LSrc := RemoveSection(LSrc, 'HTTP_VERSION');
        LSrc := RemoveSection(LSrc, '!defined(HTTPAPI_LINKAGE)');
        LSrc := RemoveSection(LSrc, '__SECSTATUS_DEFINED__');
        LSrc := ALStringReplaceA(LSrc,' HTTPAPI_LINKAGE'#13#10,' WINAPI'#13#10);
        LSrc := ALStringReplaceA(LSrc,'ULONG Present:1;','ULONG Present;');
        LSrc := ALStringReplaceA(LSrc,'[HttpHeaderRequestMaximum]','[Ord(HttpHeaderRequestMaximum)]');
        LSrc := ALStringReplaceA(LSrc,'[HttpHeaderResponseMaximum]','[Ord(HttpHeaderResponseMaximum)]');
        LSrc := ALStringReplaceA(LSrc,'[HttpRequestSizingTypeMax]','[Ord(HttpRequestSizingTypeMax)]');
        LSrc := ALStringReplaceA(LSrc,'[HttpRequestTimingTypeMax]','[Ord(HttpRequestTimingTypeMax)]');
        LSrc := ALStringReplaceA(LSrc,'HttpFeaturemax = 0xFFFFFFFF,','HttpFeaturemax = Integer($FFFFFFFF),');
        LSrc := ALStringReplaceA(LSrc,'// #define HTTPAPI_VERSION_2 { 2, 0 }','const HTTPAPI_VERSION_2: HTTPAPI_VERSION = (HttpApiMajorVersion:2; HttpApiMinorVersion:0);');
        LSrc := ALStringReplaceA(LSrc,'// #define HTTPAPI_VERSION_1 { 1, 0 }','const HTTPAPI_VERSION_1: HTTPAPI_VERSION = (HttpApiMajorVersion:1; HttpApiMinorVersion:0);');
      end;
      {$ENDREGION}

      {$REGION 'winerror.h'}
      if LChoice = 4 then begin
        LSrc := RemoveSection(LSrc, '_WINERROR_');
        LSrc := RemoveSection(LSrc, '!defined (_MSC_VER) && (_MSC_VER >= 1020) && !defined(__midl)');
        LSrc := RemoveSection(LSrc, '!FORCEINLINE');
        LSrc := RemoveSection(LSrc, 'WSABASEERR');
        LSrc := RemoveSection(LSrc, '!defined(_HRESULT_DEFINED) && !defined(__midl)');
        LSrc := RemoveSection(LSrc, '!__midl');
        LSrc := RemoveSection(LSrc, '!RC_INVOKED');
        LSrc := RemoveSection(LSrc, 'WIN_OMIT_TSS_TPM_RETURN_CODES');
        LSrc := AlStringReplaceA(LSrc, 'WINAPI', 'W!I!N!A!P!I');
      end;
      {$ENDREGION}

      {$REGION 'wincrypt.h'}
      //LSrc := ALStringReplaceA(LSrc,' WINADVAPI'#13#10,' WINAPI'#13#10);
      //LSrc := ALStringReplaceA(LSrc,' WINCRYPT32API'#13#10,' WINAPI'#13#10);
      //LSrc := ALStringReplaceA(LSrc,' WINCRYPT32STRINGAPI'#13#10,' WINAPI'#13#10);
      //LSrc := RemoveSection(LSrc, '__WINCRYPT_H__');
      //LSrc := RemoveSection(LSrc, '_HRESULT_DEFINED');
      //LSrc := RemoveSection(LSrc, '!defined(WINCRYPT32API)');
      //LSrc := RemoveSection(LSrc, '!defined(_CRYPT32_)');
      //LSrc := RemoveSection(LSrc, '!defined(WINCRYPT32STRINGAPI)');
      //LSrc := RemoveSection(LSrc, '!WINADVAPI');
      //LSrc := RemoveSection(LSrc, '!WINAPI');
      //LSrc := RemoveSection(LSrc, '!CALLBACK');
      //LSrc := RemoveSection(LSrc, '!DECLSPEC_IMPORT');
      //LSrc := RemoveSection(LSrc, '!CONST');
      //LSrc := RemoveSection(LSrc, '!IN');
      //LSrc := RemoveSection(LSrc, '!OUT');
      //LSrc := RemoveSection(LSrc, '!OPTIONAL');
      //LSrc := RemoveSection(LSrc, '!CMSG_ENVELOPED_ENCODE_INFO_HAS_CMS_FIELDS');
      //LSrc := RemoveSection(LSrc, 'defined(_DDK_DRIVER_)');
      //LSrc := ALStringReplaceA(
      //          LSrc,
      //          '// typedef struct _CMSG_RECIPIENT_ENCODE_INFO CMSG_RECIPIENT_ENCODE_INFO,'#13#10+
      //          '// * PCMSG_RECIPIENT_ENCODE_INFO;',
      //          '// typedef struct _CMSG_RECIPIENT_ENCODE_INFO'#13#10+
      //          '// {'#13#10+
      //          '// } CMSG_RECIPIENT_ENCODE_INFO, * PCMSG_RECIPIENT_ENCODE_INFO;', []);
      //LSrc := ALStringReplaceA(
      //          LSrc,
      //          '// typedef struct _CERT_REVOCATION_CHAIN_PARA'#13#10+
      //          '// CERT_REVOCATION_CHAIN_PARA,'#13#10+
      //          '// * PCERT_REVOCATION_CHAIN_PARA;',
      //          '// typedef struct _CERT_REVOCATION_CHAIN_PARA'#13#10+
      //          '// {'#13#10+
      //          '// } CERT_REVOCATION_CHAIN_PARA, * PCERT_REVOCATION_CHAIN_PARA;', []);
      //LSrc := ALStringReplaceA(
      //          LSrc,
      //          '// typedef struct _CERT_CHAIN_CONTEXT CERT_CHAIN_CONTEXT, * PCERT_CHAIN_CONTEXT;',
      //          '// typedef struct _CERT_CHAIN_CONTEXT'#13#10+
      //          '// {'#13#10+
      //          '// } CERT_CHAIN_CONTEXT, * PCERT_CHAIN_CONTEXT;');
      //LSrc := ALStringReplaceA(
      //          LSrc,
      //          '// typedef struct _CERT_SERVER_OCSP_RESPONSE_CONTEXT'#13#10+
      //          '// CERT_SERVER_OCSP_RESPONSE_CONTEXT,'#13#10+
      //          '// * PCERT_SERVER_OCSP_RESPONSE_CONTEXT;',
      //          '// typedef struct _CERT_SERVER_OCSP_RESPONSE_CONTEXT'#13#10+
      //          '// {'#13#10+
      //          '// } CERT_SERVER_OCSP_RESPONSE_CONTEXT, * PCERT_SERVER_OCSP_RESPONSE_CONTEXT;');
      //LSrc := ALStringReplaceA(LSrc, '#define AUTHTYPE_CLIENT 1','// #define AUTHTYPE_CLIENT 1');
      //LSrc := ALStringReplaceA(LSrc, '#define AUTHTYPE_SERVER 2','// #define AUTHTYPE_SERVER 2');
      //LSrc := ALStringReplaceA(LSrc, 'BOOL WINAPI CryptDllCreateCOMObject', 'BOOL W!I!N!A!P!I CryptDllCreateCOMObject');
      //LSrc := ALStringReplaceA(LSrc, 'BOOL WINAPI SchemeDllRetrieveEncodedObjectW', 'BOOL W!I!N!A!P!I SchemeDllRetrieveEncodedObjectW');
      //LSrc := ALStringReplaceA(LSrc, 'BOOL WINAPI ContextDllCreateObjectContext', 'BOOL W!I!N!A!P!I ContextDllCreateObjectContext');
      //LSrc := ALStringReplaceA(LSrc,'(CALLBACK * ','(WINAPI * ');
      {$ENDREGION}

      LSrc := ALStringReplaceA(LSrc,' (WINAPI * ',#13#10'// (WINAPI * ');

      while alposA('  ', LSrc) > 0 do
        LSrc := ALStringReplaceA(LSrc,'  ', ' ');

      //
      // ALSaveStringTofile(LSrc, ALGetModulePathW + 'debug_src.txt');
      //

      LSrc := ConvertDefine(LSrc);
      LSrc := ConvertEnums(LSrc);
      LSrc := ConvertSimpleType(LSrc);
      LSrc := ConvertRecords(LSrc);
      LSrc := Convertfunctions(LSrc);

      LSrc := AlStringReplaceA(LSrc, 'W!I!N!A!P!I', 'WINAPI');

      LSrc := '''
        unit
        ''' +
        ' ' + ALExtractFileName(AnsiString(LDstfileName), true{RemoveFileExt}) + ';' + #13#10 +
        '''

        //******************************************************************//
        // This unit was automatically generated by WinApiWrapperGenerator. //
        // Do not edit manually.                                            //
        //******************************************************************//

        interface

        {$I Alcinoe.inc}

        {$SCOPEDENUMS OFF}
        {$MINENUMSIZE 4}
        {$A+}
        '''+
        #13#10+
        #13#10+
        'uses'#13#10+
        '  WinApi.Windows' + ALIfThenA(LExtraUses <> '', ','#13#10, ';'#13#10) +
        LExtraUses + #13#10 +
        #13#10+
        LExtraTypes + #13#10+
        #13#10+
        '' + LSrc + #13#10 +
        '''

        implementation

        end.
        ''';

      var LLst := TALStringListA.Create;
      try
        LLst.Text := LSrc;
        For var i := LLst.Count - 1 downto 0 do begin
          LLst[I] := ALTrimRight(LLst[I]);
          if ALTrim(LLst[I]) = '//' then LLst[I] := ''
          else if ALPosA('// #pragma ', ALTrim(LLst[I])) = 1 then LLst[I] := ''
          else if ALPosA('// #include ', ALTrim(LLst[I])) = 1 then LLst[I] := ''
          else if (I > 0) and (AlposA('): ', ALTrim(LLst[I])) = 1) then begin
            var LStr := LLst[I-1];
            var LComment: AnsiString := '';
            var P1 := AlposA(' //', LStr, 4);
            if P1 <= 0 then P1 := AlposA(' (*', LStr, 4);
            if P1 > 0 then begin
              LComment := ALTrim(AlcopyStr(LStr, P1, ALMaxInt));
              LStr := ALTrimRight(AlcopyStr(LStr, 1, P1-1));
            end;
            LLst[I-1] := LStr + ALTrim(LLst[I]) + ALIfThenA(LComment <> '', ' ') + LComment;
            LLst.Delete(i);
          end
          else if (I > 0) and (AlposA(');', ALTrim(LLst[I])) = 1) then begin
            var LStr := LLst[I-1];
            var LComment: AnsiString := '';
            var P1 := AlposA(' //', LStr, 4);
            if P1 <= 0 then P1 := AlposA(' (*', LStr, 4);
            if P1 > 0 then begin
              LComment := ALTrim(AlcopyStr(LStr, P1, ALMaxInt));
              LStr := ALTrimRight(AlcopyStr(LStr, 1, P1-1));
            end;
            if (LStr <> '') and (LStr[high(LStr)] <> ';') then begin
              LLst[I-1] := LStr + ALTrim(LLst[I]) + ALIfThenA(LComment <> '', ' ') + LComment;
              LLst.Delete(i);
            end;
          end;
        end;
        LSrc := ALTrim(LLst.Text);
      finally
        AlFreeAndNil(LLst);
      end;
      LSrc := ALStringReplaceA(LSrc,#13#10'// //',#13#10'//',[RfReplaceALL]);
      while ALposA('/ /', LSrc) > 0 do
        LSrc := ALStringReplaceA(LSrc,'/ /','//',[RfReplaceALL]);
      while ALposA(#13#10#13#10#13#10, LSrc) > 0 do
        LSrc := ALStringReplaceA(LSrc,#13#10#13#10#13#10,#13#10#13#10,[RfReplaceALL]);

      ALSaveStringToFile(LSrc, LDstfileName);

    end;

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