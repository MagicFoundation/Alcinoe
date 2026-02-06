program CHeaderWrapperGenerator;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.Generics.Defaults,
  System.Generics.Collections,
  system.AnsiStrings,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  WinApi.Windows,
  Alcinoe.FileUtils,
  Alcinoe.StringList,
  Alcinoe.StringUtils,
  Alcinoe.common;

const
  CandidateDllNames: array[0..7] of PChar = (
    'httpapi.dll',
    'advapi32.dll',
    'crypt32.dll',
    'secur32.dll',
    'schannel.dll',
    'sspicli.dll',
    'bson2.dll',
    'mongoc2.dll');
var
  CandidateDllModules: array[0..7] of HModule;

{*******************************************************************************************}
function AddDllDirectory(NewDirectory: PWideChar): THandle; stdcall; external 'kernel32.dll';
function SetDefaultDllDirectories(DirectoryFlags: DWORD): BOOL; stdcall; stdcall; external 'kernel32.dll';

{************************}
function ConvertFunctions(
           const ACSource: AnsiString;
           const ACallingConvention: AnsiString;
           const ADelayed: Boolean;
           const ALibraryDLLEntryPoints: TALStringListA): AnsiString; forward;

{***********************************************************************************************************************}
function ReplaceSection(const ACSource: AnsiString; const ADefine: AnsiString; const ANewDefine: AnsiString): AnsiString;
begin

  //
  // // #if _CREDUI_INFO_DEFINED // ntifs
  // //     _In_opt_ PCREDUI_INFOA pUiInfo,
  // // #else
  // //     _In_opt_ PVOID pUiInfo,
  // // #endif // _CREDUI_INFO_DEFINED
  //

  var LInOut := TALStringListA.Create;
  try

    LInOut.Text := ACSource;
    var LSkipDepth: Integer := 0;

    for var i := 0 to LInOut.Count - 1 do begin
      var LLine := LInOut[i]; // // #if _CREDUI_INFO_DEFINED // ntifs
      var LTrim := ALTrim(LLine); // // #if _CREDUI_INFO_DEFINED // ntifs
      var P1 := AlposA(' //', LTrim, 4);
      if P1 > 0 then LTrim := ALTrim(AlcopyStr(LTrim, 1, P1-1)); // // #if _CREDUI_INFO_DEFINED

      // Start of a block to skip?
      // // #if _CREDUI_INFO_DEFINED
      if (LSkipDepth = 0) then begin
        if (ALPosIgnoreCaseA(LTrim, '// #if ' + ADefine) = 1) and
           (length(LTrim) >= length('// #if ' + ADefine)) then begin
          if (Not ALSameTextA(LTrim, '// #if ' + ADefine)) then
            Raise Exception.Create('Error 0C3C19A2-2C8D-4DF6-9A4A-07924414F0C3 - ' + String(LTrim));
          Inc(LSkipDepth);
          LInOut[i] := '{$if ' + ANewDefine + '}';
          Continue;
        end;
      end;

      // We are currently skipping a block that started with "#if ... LDefine ..."
      if LSkipDepth > 0 then begin
        // If we hit an #else at the *top* of this skipped block,
        // we stop skipping and keep everything from here on.
        if (LSkipDepth = 1) and
           (ALPosIgnoreCaseA('// #else', LTrim) = 1) then begin
          LInOut[i] := '{$else}';
          Continue;
        end;

        // Nested #if inside the skipped block
        if (ALPosIgnoreCaseA('// #if', LTrim) = 1) then begin
          Inc(LSkipDepth);
          Continue;
        end;

        // Matching #endif (possibly for nested levels)
        if (ALPosIgnoreCaseA('// #endif', LTrim) = 1) then begin
          Dec(LSkipDepth);
          If (LSkipDepth = 0) then LInOut[i] := '{$endif}';
          Continue;
        end;

        // Any normal line inside the skipped region
        Continue;
      end;

    end;

    Result := LInOut.Text;

  finally
    ALFreeAndNil(LInOut);
  end;

end;

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

  if Result = 'BOOLEAN' then Result := 'ByteBool'
  else if Result = 'BOOL' then Result := 'LongBool'
  else if Result = 'bool' then Result := 'ByteBool'
  else if ALSameTextA(Result, 'char') then Result := 'AnsiChar'
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
  else if ALSameTextA(AVarName, 'result') then Result := '_'+AVarName
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
            for var J := 4 to LLst.Count - 1 do
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
            P1 := ALPosIgnoreCaseA('(size_t)', Lvalue);
            if P1 > 0 then begin
              Lvalue := ALStringReplaceA(Lvalue, '(size_t)', '');
              if (Lvalue <> '') and (LValue[low(Lvalue)] <> '(') then Lvalue := '(' + Lvalue + ')';
              Lvalue := 'size_t' + Lvalue;
              LLst[2] := LLst[2] + ': size_t';
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
                (AlposA('size_t(',Lvalue) = 1) or
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
              LValue := ALStringReplaceA(Lvalue, ' << ', ' shl ');
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

      Var LSkippedType: AnsiString := '';

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
          if not ALSameTextA(LStr, ABaseCType) then begin
            Result := Result + APrefix + LStr + ' = ' + CType2Delphi(ABaseCType, true{AUseCaret}) + ';' + ALIfThenA(AComment <> '', ' ' + AComment) + #13#10;
            LLst2.Add(LStr);
          end
          else
            LSkippedType := LStr;
        end;
      end;

      if ((AlposA('P' + LSkippedType, ACSource) <= 0) and
          (AlposA('P' + LSkippedType, Result) <= 0) and
          (AlposA(LSkippedType + ' * ', ACSource) > 0)) then begin
        Result := Result + APrefix + 'P' + LSkippedType + ' = ^' + LSkippedType + ';' + ALIfThenA(AComment <> '', ' ' + AComment) + #13#10;
      end;
      if ((AlposA('PP' + LSkippedType, ACSource) <= 0) and
          (AlposA('PP' + LSkippedType, Result) <= 0) and
          (AlposA(LSkippedType + ' * * ', ACSource) > 0)) then begin
        Result := Result + APrefix + 'PP' + LSkippedType + ' = ^P' + LSkippedType + ';' + ALIfThenA(AComment <> '', ' ' + AComment) + #13#10;
      end;

      for var I := 0 to LLst2.Count - 1 do begin
        if ((not ALSameTextA(LLst2[I], LSkippedType)) and
            (AlposA('P' + LLst2[I], ACSource) <= 0) and
            (AlposA('P' + LLst2[I], Result) <= 0) and
            (AlposA(LLst2[I] + ' * ', ACSource) > 0)) then begin
          Result := Result + APrefix + 'P' + LLst2[I] + ' = ^' + LLst2[I] + ';' + ALIfThenA(AComment <> '', ' ' + AComment) + #13#10;
        end;
        if ((not ALSameTextA(LLst2[I], LSkippedType)) and
            (AlposA('PP' + LLst2[I], ACSource) <= 0) and
            (AlposA('PP' + LLst2[I], Result) <= 0) and
            (AlposA(LLst2[I] + ' * * ', ACSource) > 0)) then begin
          Result := Result + APrefix + 'PP' + LLst2[I] + ' = ^P' + LLst2[I] + ';' + ALIfThenA(AComment <> '', ' ' + AComment) + #13#10;
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

{******************************************************************************************************************************************************************************************}
function ConvertCParamsToDelphi(const ACParams: AnsiString; Const AIsFromStruct: Boolean; const ACallingConvention: AnsiString; const ADelayed: Boolean; out AVarArgs: Boolean): AnsiString;
begin

  AVarArgs := False;
  var LCParams := ACParams;
  LCParams := ALStringReplaceA(LCParams, #13#10'(__CALLCONV__', ' (__CALLCONV__');
  var LParamsLst := TALStringListA.Create;
  try
    LParamsLst.Text := ALTrim(LCParams);

    For Var i := LParamsLst.Count - 1 downto 0 do
      if LParamsLst[I] = '' then LParamsLst.Delete(i);
    For Var i := LParamsLst.Count - 1 downto 0 do
      if LParamsLst[I] = '//' then LParamsLst.Delete(i);

    var LCaseOfIndex: Integer := -1;
    var LCaseOfStruct: Boolean := False;

    For Var i := 0 to LParamsLst.Count - 1 do begin
      var LLine := ALTrim(LParamsLst[I]); // _In_opt_ LPSTR pszPrincipal, // Name of principal
      if LLine = '' then continue;
      if ALPosA('//', LLine) = 1 then begin
        LParamsLst[I] := LLine;
        if LCaseOfIndex >= 0 then LParamsLst[I] := '  ' + LLine;
        continue;
      end;
      if ALPosA('(*', LLine) = 1 then begin
        LParamsLst[I] := LLine;
        if LCaseOfIndex >= 0 then LParamsLst[I] := '  ' + LLine;
        If AlposA('*)', LLine) <= 0 then begin
          For var J := I + 1 to LParamsLst.Count - 1 do begin
            LParamsLst[I] := LParamsLst[I] + #13#10 + ALIfThenA(LCaseOfIndex >= 0, '  ') + ALIfThenA(AlposA('*', LParamsLst[J]) = 1, ' ') + LParamsLst[J];
            If AlposA('*)', LParamsLst[J]) >= 0 then begin
              LParamsLst[J] := '';
              break;
            end
            else
              LParamsLst[J] := '';
          end;
        end;
        continue;
      end;
      var LComment: AnsiString := '';
      var P1 := AlposA(' //', LLine);
      if P1 <= 0 then P1 := AlposA(' (*', LLine);
      if P1 > 0 then begin
        LComment := ALTrim(AlcopyStr(LLine, P1, ALMaxInt)); // // Name of principal
        LLine := ALTrim(AlcopyStr(LLine, 1, P1-1)); // _In_opt_ LPSTR pszPrincipal,
      end;
      if (ALPosA('__CALLCONV__', LLine) > 0) and (AlposA(';', LLine) <= 0) then begin
        LParamsLst[I] := '';
        if I = LParamsLst.Count - 1 then
          Raise Exception.Create('Error 597EEEEB-3483-4CEA-A4F6-77227A1A6591 - ' + String(LLine));
        LParamsLst[I+1] := LLine + ' ' + LParamsLst[I+1];
        continue;
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
      If (LCaseOfIndex <> -1) and (not LCaseOfStruct) and (AlposA('}', LLine) = 1) then begin
        if ALSameTextA(LLine, '}') then begin
          LParamsLst[I] := '';
          LCaseOfIndex := -1;
          Continue;
        end
        else begin
          Var LLst := TalStringListA.create;
          Try
            LLst.LineBreak := ' ';
            LLst.Text := LLine;
            If LLst.Count <> 2 then
              Raise Exception.Create('Error E33CF4CC-9C2C-4720-A38A-F77111995264 - ' + String(LLine));
            LParamsLst[I] := 'end;' + ALIfThenA(LComment <> '', ' ' + LComment);
            For var J := I - 1 downto 0 do begin
              If alposA('case Integer of', LParamsLst[J]) = 1 then begin
                LParamsLst[J] := LLst[1] + ': record' + #13#10 + '  '+LParamsLst[J];
                Break;
              end
              else begin
                LParamsLst[J] := '  '+ALStringReplaceA(LParamsLst[J], #13#10, #13#10'  ');
              end;
            end;
            LCaseOfIndex := -1;
            Continue;
          Finally
            ALFreeAndNil(LLst);
          End;
        end;
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
      if ALposA('__CALLCONV__', LLine) > 0 then begin
        // bool(__CALLCONV__ * visit_before)(const bson_iter_t * iter, char * key, void * data)

        LLine := '// typedef ' + LLine + ';'; // // typedef bool(__CALLCONV__ * visit_before)(const bson_iter_t * iter, char * key, void * data);
        P1 := ALposA('(', LLine);
        if P1 <= 0 then raise Exception.Create('Error 448A57E4-0729-46C9-BC24-4A09E1D85AF9');
        Insert(#13#10'// ', LLine, P1); // // typedef bool
                                        // // (__CALLCONV__ * visit_before)(const bson_iter_t * iter, char * key, void * data)
        P1 := ALposA('(', LLine, P1 + length(#13#10'// ')+1);
        if P1 <= 0 then
          raise Exception.Create('Error 2237650F-42CC-4FC4-B6D3-9FA008A3F6B1');
        Insert(#13#10'// ', LLine, P1); // // typedef bool
                                        // // (__CALLCONV__ * visit_before)
                                        // // (const bson_iter_t * iter, char * key, void * data)
        LLine := ConvertFunctions(
                   ';' + LLine,
                   ACallingConvention,
                   ADelayed,
                   nil{ALibraryDLLEntryPoints}); // ;
                                                 // type
                                                 //   visit_before = function (
                                                 //     iter: Pbson_iter_t;
                                                 //     key: PAnsiChar;
                                                 //     data: Pvoid
                                                 //   ): bool; stdcall;
        LLine := ALStringReplaceA(LLine, #13#10, ' ');
        while alposA('  ', LLine) > 0 do
          LLine := ALStringReplaceA(LLine,'  ', ' ');
        LLine := ALStringReplaceA(LLine, '; type ', '', []);
        LLine := ALStringReplaceA(LLine, ' (', '(');
        LLine := ALStringReplaceA(LLine, '( ', '(');
        LLine := ALStringReplaceA(LLine, ' )', ')');
        LLine := ALStringReplaceA(LLine, ') ', ')');
        LLine := ALStringReplaceA(LLine, ' = ', ': ', []);
        LParamsLst[I] := ALTrim(LLine);
      end
      else if ALposA(' * * ', LLine) > 0 then begin
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
          if ALPosA('[', LLine) > 0 then begin
            LParamsLst[I] := LLst[1] + ' of ' + CType2Delphi('^' + LLst[0], false{AUseCaret});
            LParamsLst[I] := ALStringReplaceA(LParamsLst[I], '[', ': array[0..');
            LParamsLst[I] := ALStringReplaceA(LParamsLst[I], ']', '-1]');
            LParamsLst[I] := ALStringReplaceA(LParamsLst[I], ']: array[', ', ');
            LParamsLst[I] := LParamsLst[I] + ALIfThenA(AIsFromStruct or (I < LParamsLst.Count - 1), ';') + ALIfThenA(LComment <> '', ' ' + LComment);
          end
          else
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
          if (LLst.count = 1) and (LLst[0] = '...') and (I = LParamsLst.Count - 1) then begin
            LParamsLst[I] := '';
            for var J := I - 1 downto 0 do begin
              if alposA(';', LParamsLst[J]) > 0 then begin
                LParamsLst[J] := ALStringReplaceA(LParamsLst[J], ';', '', []);
                break;
              end;
            end;
            AVarArgs := True;
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
          else
            LParamsLst[I] := CIdentifier2Delphi(LLst[1]) + ': ' + CType2Delphi(LLst[0], false{AUseCaret}) + ALIfThenA(AIsFromStruct or (I < LParamsLst.Count - 1), ';') + ALIfThenA(LComment <> '', ' ' + LComment);
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

    For Var i := LParamsLst.Count - 1 downto 0 do
      if LParamsLst[I] = '' then LParamsLst.Delete(i);

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
         (ALPosA('(__CALLCONV__ ', LLine + ' ') <= 0) and
         (ALPosA(' union ', LLine + ' ') <= 0) and
         (ALPosA(' struct ', LLine + ' ') <= 0) and
         (ALPosA(' enum ', LLine + ' ') <= 0) then begin

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

{***********************************************************************************************************************************************************}
function _ConvertEnumsOrRecords(const ACSource: AnsiString; Const AEnum: Boolean; const ACallingConvention: AnsiString; const ADelayed: Boolean): AnsiString;
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
        var P3 := alposA(#13#10,Result, P1 + 1);
        var P4 := alposA(';',Result, P1 + 1);
        if P4 < P3 then begin // typedef struct _bson_json_reader_t bson_json_reader_t;
          var LLst := TalStringListA.Create;
          try
            LLst.LineBreak := ' ';
            LLst.Text := ALTrim(AlCopyStr(Result, P1, P4-P1+1)); // //
                                                                 // typedef
                                                                 // struct
                                                                 // _bson_json_reader_t
                                                                 // bson_json_reader_t;
            if LLst.Count <> 5 then raise Exception.Create('Error 57D95569-533F-43DB-A4BD-CEE6F6434CB3');
            LLst.Delete(0);
            LLst.Delete(0);
            LLst.Delete(0); // _bson_json_reader_t
                            // bson_json_reader_t;
            LLst.Insert(1, '// {');
            LLst[2] := '// } ' + ALTrim(LLst[2]); // _bson_json_reader_t
                                                  // {
                                                  // } bson_json_reader_t;
            delete(Result, P1, P4-P1+1);
            LLst.LineBreak := #13#10;
            Insert('// typedef struct ' + ALTrim(LLst.Text), Result, P1);
            P2 := alposA(#13#10'// }',Result, P1 + 1);
          finally
            ALFreeAndNil(LLst);
          end;
        end
        else begin
          P2 := P1 + 1;
          P4 := 1;
          While P2 <= length(Result) do begin
            if alposA(#13#10'// union {', Result, P2) = P2 then inc(P4)
            else if alposA(#13#10'// struct {', Result, P2) = P2 then inc(P4)
            else if alposA(#13#10'// }',Result, P2) = P2 then dec(P4);
            if P4 = 0 then break
            else inc(P2);
          end;
        end;
      end;
      if P2 <= 0 then raise Exception.Create('Error 0DCAB0CB-D0C3-42E9-9395-1080847A491D');
      LTypeDef.Text := ALTrim(AlCopyStr(Result, P1, P2-P1)); // // typedef enum _SEC_TRAFFIC_SECRET_TYPE
                                                             // // {
                                                             // //  SecTrafficSecret_None,
                                                             // //  SecTrafficSecret_Client,
                                                             // //  SecTrafficSecret_Server
      if LTypeDef.Count < 2 then
        raise Exception.Create('Error D287A617-DFB9-49A7-99A2-BA881693B157 - ' + string(LTypeDef.Text));
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
      var LIsAnomTypeName := False;
      if LTypeName = '' then begin
        LIsAnomTypeName := true;
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
        var LVarArgs: Boolean;
        LTypeDef.Text := ConvertCParamsToDelphi(LTypeDef.Text, true{AIsFromStruct}, ACallingConvention, ADelayed, LVarArgs);
        if LVarArgs then raise Exception.Create('Error B4CF6110-A482-41CC-B08C-EE76B311BAE8');
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
        LTypeDef.Text := ALStringReplaceA(LTypeDef.Text, ' | ', ' or ');
        LTypeDef.Text := ALStringReplaceA(LTypeDef.Text, ' << ', ' shl ');
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

      var LPrevTypeName: ansiString := LTypeName;
      if LIsAnomTypeName then begin
        var LLst := TALStringListA.Create;
        try
          LLst.Text := LStr;
          LLst.TrailingLineBreak := False;
          if (LLst.Count > 0) and (ALTrim(LLst.ValueFromIndex[0]) = LTypeName + ';') then begin
            LTypeName := ALTrim(LLst.Names[0]);
            LLst.Delete(0);
            LStr := LLst.Text;
          end;
        finally
          ALFreeAndNil(LLst);
        end;
      end;

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
      if LPrevTypeName <> LTypeName then begin
        LTypeDefStr := ALStringReplaceA(LTypeDefStr, ' ' + LPrevTypeName + ' ', ' ' + LTypeName + ' ');
        LTypeDefStr := ALStringReplaceA(LTypeDefStr, ' ' + LPrevTypeName + ';', ' ' + LTypeName + ';');
        LTypeDefStr := ALStringReplaceA(LTypeDefStr, ' ^' + LPrevTypeName + ' ', ' ^' + LTypeName + ' ');
        LTypeDefStr := ALStringReplaceA(LTypeDefStr, ' ^' + LPrevTypeName + ';', ' ^' + LTypeName + ';');
      end;

      delete(Result,P1, P3-P1);
      insert(#13#10+LTypeDefStr,Result,P1);

      P1 := alposA(#13#10'// typedef '+LTypedefName+' ',Result, P1+1);

    finally
      ALFreeAndNil(LTypeDef);
    end;
  end;

end;

{***************************************************************************************************************************}
function ConvertEnums(const ACSource: AnsiString; const ACallingConvention: AnsiString; const ADelayed: Boolean): AnsiString;
begin
  Result := _ConvertEnumsOrRecords(ACSource, true{AEnum}, ACallingConvention, ADelayed);
end;

{*****************************************************************************************************************************}
function ConvertRecords(const ACSource: AnsiString; const ACallingConvention: AnsiString; const ADelayed: Boolean): AnsiString;
begin
  Result := _ConvertEnumsOrRecords(ACSource, false{AEnum}, ACallingConvention, ADelayed);
end;

{*************************************************************}
function GetDllImport(const AProcName: AnsiString): Ansistring;
begin
  for var I := low(CandidateDllModules) to high(CandidateDllModules) do begin
    if GetProcAddress(CandidateDllModules[I], PAnsiChar(AProcName)) <> nil then begin
      Result := ansiString(CandidateDllNames[i]);
      Exit;
    end;
  end;
  Result := '';
end;

{************************}
function ConvertFunctions(
           const ACSource: AnsiString;
           const ACallingConvention: AnsiString;
           const ADelayed: Boolean;
           const ALibraryDLLEntryPoints: TALStringListA): AnsiString;
begin

  //
  // // SECURITY_STATUS __CALLCONV__
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
  // // (__CALLCONV__ * ENCRYPT_MESSAGE_FN)(
  // //     PCtxtHandle, unsigned long, PSecBufferDesc, unsigned long);
  //

  Result := ACSource;

  var P1 := alposA('__CALLCONV__',Result);
  while P1 > 0 do begin
    if (P1 <= low(Result)) or
       (Result[P1-1] not in [#13,#10, ' ', '(']) or
       (P1 + length('__CALLCONV__') > high(Result)) or
       (Result[P1+ length('__CALLCONV__')] not in [#13,#10, ' ']) then begin
      P1 := alposA('__CALLCONV__',Result, P1+1);
      continue;
    end;
    var P2 := P1;
    while (P2 > 0) and (alposA(#13#10'// '#13#10, Result, P2) <> P2) and (Result[P2] not in ['"', '.', ';', '-', ')']) do dec(P2);
    if P2 = 0 then raise Exception.Create('Error 5C59AC01-3BB5-446B-BDE1-2E143B187E23');
    if Result[P2] in ['"', '.', ';', '-', ')'] then inc(P2)
    else inc(P2, length(#13#10'// '#13#10));
    var P3 := P1;
    while (P3 <= high(Result)) and (Result[P3] <> '(') do inc(P3);
    if P3 > high(Result) then raise Exception.Create('Error C2DDA7D8-5128-44D5-BFE7-1A2971A73177');

    var LRoutinePrefix: AnsiString := '';
    var LRoutineSuffix: AnsiString := '';
    var LRoutineName: AnsiString := '';
    var LDllImport: AnsiString := '';
    var LHeaderStr := AlCopyStr(Result, P2, P3-P2);  // // SECURITY_STATUS __CALLCONV__
                                                     // // AcquireCredentialsHandleA
    LHeaderStr := ALStringReplaceA(LHeaderStr, '(__CALLCONV__ * ', ''); // // typedef SECURITY_STATUS
                                                                  // // ENCRYPT_MESSAGE_FN)(
    LHeaderStr := ALStringReplaceA(LHeaderStr, '__CALLCONV__', ''); // // SECURITY_STATUS
                                                              // // AcquireCredentialsHandleA
    var LIsCallbackFunctionPointer := ALPosA('typedef', LHeaderStr) > 0;
    LHeaderStr := ALStringReplaceA(LHeaderStr, 'typedef', ''); // // SECURITY_STATUS
                                                               // // ENCRYPT_MESSAGE_FN)
    LHeaderStr := ALStringReplaceA(LHeaderStr, ')', ''); // // SECURITY_STATUS
                                                         // // ENCRYPT_MESSAGE_FN
    LHeaderStr := ALStringReplaceA(LHeaderStr, '// struct ', '// ');

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

      if (LHeaderLst.Count = 1) then begin // char * * mongoc_client_get_database_names
        Var LLine := ALTrim(LHeaderLst.Text);
        var P4 := High(LLine);
        While (P4 > low(LLine)) and (LLine[P4] <> ' ') do dec(P4);
        LHeaderLst[0] := ALTrim(ALCopyStr(LLine, 1, P4-1));
        LHeaderLst.Add(ALTrim(ALCopyStr(LLine, P4+1, Maxint))); // char * *
                                                                // mongoc_client_get_database_names
      end;

      if (LHeaderLst.Count <> 2) then
        raise Exception.Create('Error 30F920B9-FDF2-4483-842A-2C86C4E887AC - ' + string(LHeaderStr));

      if ALSameTextA(ALTrim(LHeaderLst[0]), 'VOID') then LRoutinePrefix := 'procedure '
      else begin
        LRoutinePrefix := 'function ';
        if ALposA(' * *', LHeaderLst[0]) > 0 then begin // char * *
          var LStr := ALTrim(ALStringReplaceA(LHeaderLst[0],' * *','',[]));
          LRoutineSuffix := ': ' + CType2Delphi('^^' + LStr, false{AUseCaret}); // : PPchar
        end
        else if ALposA(' *', LHeaderLst[0]) > 0 then begin // void *
          var LStr := ALTrim(ALStringReplaceA(LHeaderLst[0],' *','',[]));
          LRoutineSuffix := ': ' + CType2Delphi('^' + LStr, false{AUseCaret}); // : Pvoid
        end
        else
          LRoutineSuffix := ': ' + CType2Delphi(LHeaderLst[0], false{AUseCaret}); // : SECURITY_STATUS
      end;
      LRoutineName := ALTrim(LHeaderLst[1]);
      LDllImport := GetDllImport(LRoutineName);
      if (LDllImport = '') and (not LIsCallbackFunctionPointer) then begin
        P1 := alposA('__CALLCONV__',Result, P1+1);
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
      var LVarArgs: Boolean;
      LParamsLst.Text := ConvertCParamsToDelphi(LParamsLst.Text, False{AIsFromStruct}, ACallingConvention, ADelayed, LVarArgs);
      if LVarArgs and LIsCallbackFunctionPointer then
        Raise Exception.Create('Error C22E2578-3E6F-4F1C-920B-F0C3171CB710');


      var LIdent: AnsiString;
      If alposA('function ', LRoutinePrefix) > 0 then LIdent := '         '
      else LIdent := '          ';
      For Var i := LParamsLst.Count - 1 downto 1 do
        LParamsLst[I] := '  ' + ALIfThenA(LIsCallbackFunctionPointer, '  ', LIdent) + ALTrim(LParamsLst[I]);

      var LRoutineDefinition := aLTrim(LParamsLst.Text);
      if (ALibraryDLLEntryPoints <> nil) and (not LIsCallbackFunctionPointer) then begin
        LRoutineDefinition := LRoutinePrefix +
                              ALIfThenA(
                                LRoutineDefinition <> '',
                                '('#13#10'  ' + LIdent + ALTrim(LParamsLst.Text) + #13#10 +')',
                                '()') +
                              LRoutineSuffix +
                              '; '+{ACallingConvention+';'+}{ALIfThenA(LVarArgs, ' varargs; ')+}'inline;';

        delete(Result,P2, P4-P2+2{);});
        insert(#13#10+LRoutineDefinition,Result,P2);

        for var I := LParamsLst.Count - 1 downto 0 do begin
          LParamsLst[I] := ALTrim(LParamsLst[I]);
          P3 := ALPosA('//',LParamsLst[I]);
          If P3 > 0 then LParamsLst[I] := ALTrim(AlCopyStr(LParamsLst[I], 1, P3-1));
          if LParamsLst[I] = '' then LParamsLst.Delete(I);
        end;
        LParamsLst.LineBreak := ' ';
        LParamsLst.TrailingLineBreak := False;
        LRoutineDefinition := LRoutinePrefix +
                              ALIfThenA(
                                LRoutineDefinition <> '',
                                '('+ALTrim(LParamsLst.Text)+')',
                                '()') +
                              LRoutineSuffix +
                              '; '+ACallingConvention+';'{+ALIfThenA(LVarArgs, ' varargs;')}; // bson_zero_free: procedure(mem: Pvoid; size: size_t); cdecl;
        ALibraryDLLEntryPoints.AddNameValue(LDllImport, LRoutineDefinition);
      end
      else begin
        LRoutineDefinition := LRoutinePrefix +
                              ALIfThenA(
                                LRoutineDefinition <> '',
                                '('#13#10'  ' + ALIfThenA(LIsCallbackFunctionPointer, '  ', LIdent) + ALTrim(LParamsLst.Text) + #13#10+ ALIfThenA(LIsCallbackFunctionPointer, '  ') +')',
                                '()') +
                              LRoutineSuffix +
                              '; '+ACallingConvention+';'+ALIfThenA(LVarArgs, ' varargs;')+ALIfThenA(not LIsCallbackFunctionPointer, ' external '''+LDllImport+ALIfThenA(ADelayed, ''' delayed;', ''';'));

        delete(Result,P2, P4-P2+2{);});
        insert(#13#10+LRoutineDefinition,Result,P2);
      end;

    finally
      ALFReeAndNil(LParamsLst);
    end;

    P1 := alposA('__CALLCONV__',Result, P2);
  end;

end;

{***************************************************************************************************************************}
Function MakeLibraryInterface(const ALibraryClassName: AnsiString; const ALibraryDLLEntryPoints: TALStringListA): AnsiString;
begin
  if ALibraryDLLEntryPoints = nil then exit('');

  Result := '///'+AnsiString(StringOfChar('/', length(ALibraryClassName)))+'///'#13#10 +
            '// '+ALibraryClassName+' //'#13#10 +
            '///'+AnsiString(StringOfChar('/', length(ALibraryClassName)))+'///'#13#10 +
            #13#10;

  Var LDlls := TALStringListA.Create;
  try
    LDlls.Sorted := True;
    LDlls.Duplicates := TDuplicates.dupignore;
    for var I := 0 to ALibraryDLLEntryPoints.Count - 1 do
      LDlls.Add(ALibraryDLLEntryPoints.Names[i]);
    Result := Result +
              'type'#13#10+
              '  ' + ALibraryClassName + ' = class(TObject)'#13#10+
              '  private'#13#10;
    for var I := 0 to LDlls.Count - 1 do begin
      var LLibName := LDlls[I]; // Mongoc2.dll
      LLibName := 'F' + ALStringReplaceA(LLibName, '.dll', 'Lib'); // FMongoc2Lib
      Result := Result + '    ' + LLibName + ': THandle;'#13#10;
    end;
    Result := Result +
              '  public'#13#10;
    for var I := 0 to ALibraryDLLEntryPoints.Count - 1 do begin
      var LMethod := ALibraryDLLEntryPoints.ValueFromIndex[I]; // procedure mongoc_usleep_default_impl(usec: int64_t; user_data: Pvoid); cdecl;
      if AlposA('procedure ', LMethod) = 1 then begin
        LMethod := ALTrim(AlStringReplaceA(LMethod, 'procedure ', '')); // mongoc_usleep_default_impl(usec: int64_t; user_data: Pvoid); cdecl;
        var P1 := AlPosA('(',LMethod);
        if P1 <= 0 then raise Exception.Create('Error A73BAEA7-A92A-4A07-A77D-F0964FDD6D23');
        insert(': procedure', LMethod, P1);
      end
      else if AlposA('function ', LMethod) = 1 then begin
        LMethod := ALTrim(AlStringReplaceA(LMethod, 'function ', ''));
        var P1 := AlPosA('(',LMethod);
        if P1 <= 0 then raise Exception.Create('Error A73BAEA7-A92A-4A07-A77D-F0964FDD6D23');
        insert(': function', LMethod, P1);
      end
      else
        raise Exception.Create('Error 0343AEB5-2F5E-4A68-B162-E16FCF4381B7');
      Result := Result + '    ' + LMethod+#13#10;
    end;
    Result := Result +
              '  public'#13#10+
              '    constructor Create(const ALibraryDir: String); virtual;'#13#10+
              '    destructor Destroy; override;'#13#10+
              '  end;'#13#10+
              #13#10;
    Result := Result +
              'var'#13#10+
              '  '+ALcopyStr(ALibraryClassName, 2, Maxint) + ': ' + ALibraryClassName + ';'#13#10+
              #13#10+
              'procedure ALCreate'+ALcopyStr(ALibraryClassName, 2, Maxint)+'(const ALibraryDir: String);'#13#10+
              'procedure ALFree'+ALcopyStr(ALibraryClassName, 2, Maxint)+';';
  finally
    AlFreeAndNil(LDlls);
  end;
end;

{************************************************************************************************************************************************************}
Function MakeLibraryImplementation(const ALibraryClassName: AnsiString; const ALibraryDLLEntryPoints: TALStringListA; const ADstfileName: String): AnsiString;
begin
  if ALibraryDLLEntryPoints = nil then exit('');
  Var LDlls := TALStringListA.Create;
  try
    LDlls.Sorted := True;
    LDlls.Duplicates := TDuplicates.dupignore;
    for var I := 0 to ALibraryDLLEntryPoints.Count - 1 do
      LDlls.Add(ALibraryDLLEntryPoints.Names[i]);
    Result := 'uses'#13#10 +
              '  System.SysUtils,'#13#10 +
              '  System.IOUtils,'#13#10 +
              '  Alcinoe.Common;'#13#10 +
              #13#10 +
              '{' + AnsiString(StringOfChar('*', length('constructor '+ALibraryClassName+'.Create(const ALibraryDir: String);') - 2)) +'}' + #13#10 +
              'constructor '+ALibraryClassName+'.Create(const ALibraryDir: String);'#13#10+
              #13#10+
              '  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}'#13#10+
              '  function GetProcAddress(const AModule: HMODULE; const AProcName: PAnsiChar): Pointer;'#13#10+
              '  begin'#13#10+
              '    Result := Winapi.Windows.GetProcAddress(AModule, AProcName);'#13#10+
              '    if Result = nil then'#13#10+
              '      raise Exception.CreateFmt(''Function "%s" was not found in the given DLL'', [AProcName]);'#13#10+
              '  end;'#13#10+
              #13#10+
              'begin'#13#10+
              #13#10+
              '  inherited Create;'#13#10+
              #13#10;
    for var I := 0 to LDlls.Count - 1 do begin
      var LLibName := LDlls[I]; // Mongoc2.dll
      LLibName := 'F' + ALStringReplaceA(LLibName, '.dll', 'Lib'); // FMongoc2Lib
      Result := Result +
                '  '+LLibName+' := LoadLibrary(PChar(TPath.Combine(ALibraryDir, '''+LDlls[I]+''')));'#13#10+
                '  if '+LLibName+' = 0 then raiseLastOsError;'#13#10+
                #13#10;
    end;
    Result := Result +
              #13#10;
    for var I := 0 to ALibraryDLLEntryPoints.Count - 1 do begin
      var LLibName := ALibraryDLLEntryPoints.Names[I]; // Mongoc2.dll
      LLibName := 'F' + ALStringReplaceA(LLibName, '.dll', 'Lib'); // FMongoc2Lib
      var LMethodName := ALibraryDLLEntryPoints.ValueFromIndex[I];
      LMethodName := ALStringReplaceA(LMethodName, '; cdecl;', ';');
      LMethodName := ALStringReplaceA(LMethodName, 'function ', '');
      LMethodName := ALStringReplaceA(LMethodName, 'procedure ', '');
      var P1 := AlposA('(', LMethodName);
      If P1 <= 0 then raise Exception.Create('Error F71BE3A1-728F-4131-8A1C-280162809909');
      LMethodName := AlTrim(AlCopyStr(LMethodName, 1, P1-1));
      Result := Result +
                '  ' + LMethodName+' := GetProcAddress('+LLibName+','''+LMethodName+''');'#13#10;
    end;
    Result := Result +
              #13#10 +
              '  __LibraryConstructor__'#13#10+
              #13#10+
              'end;'#13#10+
              #13#10+
              '{' + AnsiString(StringOfChar('*', length('destructor '+ALibraryClassName+'.Destroy;') - 2)) +'}' + #13#10 +
              'destructor '+ALibraryClassName+'.Destroy;'#13#10+
              'begin'#13#10+
              #13#10+
              '  __LibraryDestructor__'#13#10+
              #13#10;
    for var I := 0 to LDlls.Count - 1 do begin
      var LLibName := LDlls[I]; // Mongoc2.dll
      LLibName := 'F' + ALStringReplaceA(LLibName, '.dll', 'Lib'); // FMongoc2Lib
      Result := Result +
      '  if ('+LLibName+' <> 0) and'#13#10+
      '     (not FreeLibrary('+LLibName+')) then'#13#10+
      '    raiseLastOsError;'#13#10+
      #13#10;
    end;
    Result := Result +
              #13#10+
              '  inherited Destroy;'#13#10+
              #13#10+
              'end;'#13#10+
              #13#10;
    for var I := 0 to ALibraryDLLEntryPoints.Count - 1 do begin
      var LMethodHeader := ALibraryDLLEntryPoints.ValueFromIndex[I];
      LMethodHeader := ALStringReplaceA(LMethodHeader, '; cdecl;', ';');
      var LMethodImplementation := LMethodHeader;
      if ALPosA('function ', LMethodImplementation) = 1 then
        LMethodImplementation := ALStringReplaceA(LMethodImplementation, 'function ', 'Result !=! ' + ALcopyStr(ALibraryClassName, 2, Maxint)+'.')
      else if ALPosA('procedure ', LMethodImplementation) = 1 then
        LMethodImplementation := ALStringReplaceA(LMethodImplementation, 'procedure ', ALcopyStr(ALibraryClassName, 2, Maxint)+'.')
      else Raise Exception.Create('Error 9D1739EC-F105-4015-8B5C-D53302A20AEC');
      var P1 := AlposA(':', LMethodImplementation);
      While P1 > 0 do begin
        var P2 := AlPosA(';', LMethodImplementation, P1);
        IF P2 <= 0 then raise Exception.Create('Error AB7BDA4E-A690-4A43-BA48-AAD7161FFAC9');
        var P3 := AlPosA(')', LMethodImplementation, P1);
        If (P3 > 0) and (P3 < P2) then P2 := P3;
        delete(LMethodImplementation, P1, P2-P1);
        P1 := AlposA(':', LMethodImplementation);
      end;
      LMethodImplementation := ALStringReplaceA(LMethodImplementation, ';', ',');
      LMethodImplementation := ALStringReplaceA(LMethodImplementation, '),', ');');
      LMethodImplementation := ALStringReplaceA(LMethodImplementation, 'Result !=! ', 'Result := ');
      Result := Result +
                '{' + AnsiString(StringOfChar('*', length(LMethodHeader) - 2)) +'}' + #13#10 +
                LMethodHeader + #13#10+
                'begin'#13#10 +
                '  ' + LMethodImplementation +#13#10+
                'end;'#13#10#13#10;
    end;
    Result := result +
              '{' + AnsiString(StringOfChar('*', length('procedure ALCreate'+ALcopyStr(ALibraryClassName, 2, Maxint)+'(const ALibraryDir: String);') - 2)) +'}' + #13#10 +
              'procedure ALCreate'+ALcopyStr(ALibraryClassName, 2, Maxint)+'(const ALibraryDir: String);'#13#10 +
              'begin'#13#10 +
              '  if assigned('+ALcopyStr(ALibraryClassName, 2, Maxint)+') then exit;'#13#10 +
              '  '+ALcopyStr(ALibraryClassName, 2, Maxint)+' := '+ALibraryClassName+'.Create(ALibraryDir);'#13#10 +
              'end;'#13#10 +
              ''#13#10 +
              '{' + AnsiString(StringOfChar('*', length('procedure ALFree'+ALcopyStr(ALibraryClassName, 2, Maxint)+';') - 2)) +'}' + #13#10 +
              'procedure ALFree'+ALcopyStr(ALibraryClassName, 2, Maxint)+';'#13#10 +
              'begin'#13#10 +
              '  ALFreeAndNil('+ALcopyStr(ALibraryClassName, 2, Maxint)+');'#13#10 +
              'end;'#13#10 +
              ''#13#10 +
              'initialization'#13#10 +
              '  {$IF defined(DEBUG)}'#13#10 +
              '  ALLog('''+AnsiString(ALExtractFileName(ADstfileName, true{RemoveFileExt}))+''',''initialization'');'#13#10 +
              '  {$ENDIF}'#13#10 +
              '  '+ALcopyStr(ALibraryClassName, 2, Maxint)+' := nil;'#13#10;

  finally
    AlFreeAndNil(LDlls);
  end;
end;

begin
  try

    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    if not SetDefaultDllDirectories($00001000{LOAD_LIBRARY_SEARCH_DEFAULT_DIRS}) then raiseLastOsError;
    if AddDllDirectory(PWideChar(ALGetModulePathW + '..\..\Libraries\dll\MongoDB\'+{$If defined(Win64)}'Win64'{$else}'Win32'{$endif})) = 0 then raiseLastOsError;

    for var I := Low(CandidateDllNames) to High(CandidateDllNames) do
      CandidateDllModules[I] := LoadLibraryExW(CandidateDllNames[I], 0, $00001000{LOAD_LIBRARY_SEARCH_DEFAULT_DIRS});

    try

      Writeln;
      Writeln('Which header/unit do you want to convert?');
      Writeln('  1) sspi.h      -> Alcinoe.WinApi.SSPI.pas');
      Writeln('  2) schannel.h  -> Alcinoe.WinApi.SChannel.pas');
      Writeln('  3) http.h      -> Alcinoe.WinApi.Http.pas');
      Writeln('  4) winerror.h  -> Alcinoe.WinApi.WinError.pas');
      Writeln('  5) mongodb*.h  -> Alcinoe.MongoDB.Wrapper.pas');
      Writeln;
      Write('Choice [1-5, empty = all]: ');

      var LChoiceStr: string;
      Readln(LChoiceStr);
      Var LChoices: TArray<Integer>;
      var LInt: integer;
      if ALTryStrToInt(LChoiceStr, LInt) then LChoices := [LInt]
      else LChoices := [1,2,3,4,5];

      for var LChoice in LChoices do begin

        var LSrc: AnsiString;
        var LDstfileName: String;
        var LCallingConvention: AnsiString;
        var LDelayed: Boolean;
        var LLibraryClassName: AnsiString;
        var LLibraryDLLEntryPoints: TALStringListA;
        var LExtraUses: AnsiString;
        var LExtraTypes: AnsiString;

        try

          {$REGION 'sspi.h'}
          if LChoice = 1 then begin
            Writeln('Converting sspi.h -> Alcinoe.WinApi.SSPI.pas');
            LSrc := ALGetStringFromFile(ALGetModulePathW + 'sspi.h');
            LDstfileName := ALGetModulePathW + '..\..\Source\Alcinoe.WinApi.SSPI.pas';
            LCallingConvention := 'stdcall';
            LDelayed := False;
            LLibraryClassName := '';
            LLibraryDLLEntryPoints := nil;
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
            LSrc := ALGetStringFromFile(ALGetModulePathW + 'schannel.h');
            LDstfileName := ALGetModulePathW + '..\..\Source\Alcinoe.WinApi.SChannel.pas';
            LCallingConvention := 'stdcall';
            LDelayed := False;
            LLibraryClassName := '';
            LLibraryDLLEntryPoints := nil;
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
            LSrc := ALGetStringFromFile(ALGetModulePathW + 'http.h');
            LDstfileName := ALGetModulePathW + '..\..\Source\Alcinoe.WinApi.Http.pas';
            LCallingConvention := 'stdcall';
            LDelayed := False;
            LLibraryClassName := '';
            LLibraryDLLEntryPoints := nil;
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
            LSrc := ALGetStringFromFile(ALGetModulePathW + 'winerror.h');
            LDstfileName := ALGetModulePathW + '..\..\Source\Alcinoe.WinApi.WinError.pas';
            LCallingConvention := 'stdcall';
            LDelayed := False;
            LLibraryClassName := '';
            LLibraryDLLEntryPoints := nil;
            LExtraUses := '';
            LExtraTypes := '';
          end;
          {$ENDREGION}

          {$REGION 'mongodb*.h'}
          if LChoice = 5 then begin
            Writeln('Converting mongodb*.h -> Alcinoe.MongoDB.Wrapper.pas');
            var LFiles := TDirectory.GetFiles(ALGetModulePathW + '..\..\Libraries\dll\MongoDB\Win64\include\', '*.h', TSearchOption.soAllDirectories);
            var LSortedList := TALStringListW.create;
            Try
              LSortedList.add('config.h');
              LSortedList.add('version.h');
              LSortedList.add('macros.h');
              LSortedList.add('bson_t.h');
              LSortedList.add('compat.h');
              LSortedList.add('error.h');
              LSortedList.add('memory.h');
              LSortedList.add('bson-prelude.h');
              LSortedList.add('bson-endian.h');
              LSortedList.add('bson-writer.h');
              LSortedList.add('bson-types.h');
              LSortedList.add('bson-context.h');
              LSortedList.add('bson-clock.h');
              LSortedList.add('bson-decimal128.h');
              LSortedList.add('bson-json.h');
              LSortedList.add('bson-keys.h');
              LSortedList.add('bson-string.h');
              LSortedList.add('bson-utf8.h');
              LSortedList.add('bson-value.h');
              LSortedList.add('bson-version-functions.h');
              LSortedList.add('bson-bcon.h');
              LSortedList.add('bson-reader.h');
              LSortedList.add('bson-oid.h');
              LSortedList.add('bson-iter.h');
              LSortedList.add('bson-vector.h');
              LSortedList.add('bson.h');
              LSortedList.add('mongoc-prelude.h');
              LSortedList.add('mongoc-macros.h');
              LSortedList.add('mongoc-config.h');
              LSortedList.add('mongoc-host-list.h');
              LSortedList.add('mongoc-flags.h');
              LSortedList.add('mongoc-iovec.h');
              LSortedList.add('mongoc-opcode.h');
              LSortedList.add('mongoc-version.h');
              LSortedList.add('mongoc-write-concern.h');
              LSortedList.add('mongoc-read-concern.h');
              LSortedList.add('mongoc-optional.h');
              LSortedList.add('mongoc-error.h');
              LSortedList.add('mongoc-find-and-modify.h');
              LSortedList.add('mongoc-handshake.h');
              LSortedList.add('mongoc-init.h');
              LSortedList.add('mongoc-log.h');
              LSortedList.add('mongoc-oidc-callback.h');
              LSortedList.add('mongoc-rand.h');
              LSortedList.add('mongoc-read-prefs.h');
              LSortedList.add('mongoc-server-api.h');
              LSortedList.add('mongoc-server-description.h');
              LSortedList.add('mongoc-ssl.h');
              LSortedList.add('mongoc-socket.h');
              LSortedList.add('mongoc-stream.h');
              LSortedList.add('mongoc-stream-buffered.h');
              LSortedList.add('mongoc-stream-file.h');
              LSortedList.add('mongoc-stream-socket.h');
              LSortedList.add('mongoc-structured-log.h');
              LSortedList.add('mongoc-version-functions.h');
              LSortedList.add('mongoc-cursor.h');
              LSortedList.add('mongoc-change-stream.h');
              LSortedList.add('mongoc-bulk-operation.h');
              LSortedList.add('mongoc-client-side-encryption.h');
              LSortedList.add('mongoc-stream-tls.h');
              LSortedList.add('mongoc-stream-tls-openssl.h');
              LSortedList.add('mongoc-topology-description.h');
              LSortedList.add('mongoc-apm.h');
              LSortedList.add('mongoc-uri.h');
              LSortedList.add('mongoc-collection.h');
              LSortedList.add('mongoc-database.h');
              LSortedList.add('mongoc-gridfs-file.h');
              LSortedList.add('mongoc-gridfs-file-list.h');
              LSortedList.add('mongoc-gridfs-file-page.h');
              LSortedList.add('mongoc-gridfs-bucket.h');
              LSortedList.add('mongoc-gridfs.h');
              LSortedList.add('mongoc-stream-gridfs.h');
              LSortedList.add('mongoc-client.h');
              LSortedList.add('mongoc-client-pool.h');
              LSortedList.add('mongoc-client-session.h');
              LSortedList.add('mongoc-bulkwrite.h');
              LSortedList.add('mongoc-sleep.h');
              LSortedList.add('mongoc.h');

              TArray.Sort<string>(LFiles,
                TComparer<string>.Construct(
                  function(const L, R: string): Integer
                    function GetRank(const S: String): Integer;
                    begin
                      Result := LSortedList.IndexOf(S);
                      if Result < 0 then begin
                        if AlposW('bson', S) = 1 then Result := 1000
                        else Result := 10000;
                      end;
                    end;
                  begin
                    Result := GetRank(ALExtractFilename(L)) - GetRank(ALExtractFilename(R));
                    If Result = 0 then Result := CompareText(L, R);
                  end));

            Finally
              ALFreeAndNil(LSortedList);
            End;

            LSrc := '';
            For var I := low(LFiles) to high(Lfiles) do begin
              var LFilename := ansiString(ALExtractFilename(LFiles[i], true));
              var LFileSrc := ALTrim(ALGetStringFromFile(LFiles[i]));
              LFileSrc := ALStringReplaceA(LFileSrc, '#if !'+ALStringReplaceA(LFilename,'-','_')+'_H', '#if !__FILENAME__');
              LFileSrc := ALStringReplaceA(LFileSrc, '#ifndef '+ALStringReplaceA(LFilename,'-','_')+'_H', '#ifndef __FILENAME__');
              LFileSrc := ALStringReplaceA(LFileSrc, '#define '+ALStringReplaceA(LFilename,'-','_')+'_H'#13#10, '#define __FILENAME__'#13#10);
              LFileSrc := ALStringReplaceA(LFileSrc, '#define '+ALStringReplaceA(LFilename,'-','_')+'_H'#10, '#define __FILENAME__'#10);
              var P1 := AlPosA('/*', LFileSrc);
              if P1 = 1 then begin
                var P2 := AlPosA('*/', LFileSrc);
                If P2 <= 0 then raise Exception.Create('Error 72935AAE-4B6C-4C3E-B7A9-BE6A522B41DC');
                LFileSrc := ALTrim(AlCopyStr(LFileSrc, P2 + 2, maxint));
              end;
              P1 := AlPosA('/*    Copyright 2009', LFileSrc);
              if P1 > 0 then begin
                var P2 := AlPosA('*/', LFileSrc, P1);
                If P2 <= 0 then raise Exception.Create('Error B84F9508-8815-4C09-ADD2-387C90D22FC9');
                delete(LFileSrc, P1, P2 + 2 - P1);
              end;
              LSrc := LSrc + #13#10 + #13#10 +
                      '///'+StringOfchar(AnsiChar('/'), length(LFilename))+'///' + #13#10 +
                      '// ' + LFilename + ' //' + #13#10 +
                      '///'+StringOfchar(AnsiChar('/'), length(LFilename))+'///' + #13#10 +
                      #13#10 +
                      LFileSrc;
            end;
            LDstfileName := ALGetModulePathW + '..\..\Source\Alcinoe.MongoDB.Wrapper.pas';
            LCallingConvention := 'cdecl';
            LDelayed := False;
            LLibraryClassName := 'TALMongoDBLibrary';
            LLibraryDLLEntryPoints := TALStringListA.Create;
            LExtraUses := '''
                Winapi.Winsock2,
                Alcinoe.WinApi.Windows;
              ''';
            // https://learn.microsoft.com/en-us/cpp/c-runtime-library/standard-types?view=msvc-170
            LExtraTypes := '''
              type
                uint8_t = UInt8;
                Puint8_t = ^uint8_t;
                PPuint8_t = ^Puint8_t;
                uint16_t = UInt16;
                Puint16_t = ^uint16_t;
                uint32_t = UInt32;
                Puint32_t = ^uint32_t;
                uint64_t = UInt64;
                Puint64_t = ^uint64_t;
                int8_t = Int8;
                Pint8_t = ^int8_t;
                int16_t = Int16;
                Pint16_t = ^int16_t;
                int32_t = Int32;
                Pint32_t = ^int32_t;
                int64_t = Int64;
                Pint64_t = ^int64_t;
                size_t = NativeUInt;
                Psize_t = ^size_t;
                socklen_t = Int32;
                Psocklen_t = ^socklen_t;
                Pva_list = ^va_list;
                off_t = LongInt;
                time_t = Int64;
                TCharArray6 = array[0..6-1] of AnsiChar;
                TCharArray25 = array[0..25-1] of AnsiChar;
                P_bson_array_builder_t = pointer; // forward decl
                P_mongoc_client_t = pointer; // forward decl
                P_mongoc_client_session_t = pointer; // forward decl
                P_mongoc_client_pool_t = pointer; // forward decl
                P_mongoc_cursor_t = pointer; // forward decl
                P_mongoc_database_t = pointer; // forward decl
                P_mongoc_collection_t = pointer; // forward decl
              ''';
          end;
          {$ENDREGION}

          {$REGION 'wincrypt.h'}
          //LSrc := ALGetStringFromFile(ALGetModulePathW + 'wincrypt.h');
          //LDstfileName := ALGetModulePathW + '..\..\Source\Alcinoe.WinApi.WinCrypt.pas';
          //LCallingConvention := 'stdcall';
          //LDelayed := False;
          //LLibraryClassName := '';
          //LLibraryDLLEntryPoints := nil;
          //LExtraUses := '';
          //LExtraTypes := '';
          {$ENDREGION}

          LSrc := ALTrim(LSrc);
          LSrc := AlUTF8removeBOM(LSrc);

          var LLst := TALStringListA.Create;
          try
            LLst.Text := LSrc;
            var LbracketCount := 0;
            For var i := 2 to LLst.Count - 1 do begin
              if (LLst[I] = '{') and ((AlposA(' static ', ' ' + LLst[I-1]) > 0) or (AlposA(' static ', ' ' + LLst[I-2]) > 0) or
                                      (AlposA(' static ', ' ' + LLst[I-3]) > 0) or (AlposA(' static ', ' ' + LLst[I-4]) > 0) or
                                      (AlposA(' static ', ' ' + LLst[I-5]) > 0) or (AlposA(' static ', ' ' + LLst[I-5]) > 0)) then begin
                inc(LbracketCount);
                LLst[I] := '';
              end;
              if (AlposA('}', LLst[I]) = 1) and (LbracketCount > 0) then begin
                Dec(LbracketCount);
                LLst[I] := '';
              end;
              if LbracketCount > 0 then LLst[I] := '';
            end;
            LSrc := ALTrim(LLst.Text);
          finally
            AlFreeAndNil(LLst);
          end;

          var P1 := AlPosA('/*', LSrc);
          if P1 = 1 then begin
            var P2 := AlPosA('*/', LSrc);
            If P2 <= 0 then raise Exception.Create('Error 28C67A89-F0E7-44B0-9D74-C44580B4DA03');
            LSrc := ALTrim(AlCopyStr(LSrc, P2 + 2, maxint));
          end
          else begin
            P1 := AlPosA('//+', LSrc);
            if P1 = 1 then begin
              var P2 := AlPosA('//-', LSrc);
              If P2 <= 0 then raise Exception.Create('Error 0B153ECD-5D9E-418C-8625-93F97ECB5379');
              P2 := AlPosA('-'#13#10, LSrc, P2);
              If P2 <= 0 then raise Exception.Create('Error 7115E9FD-893A-441E-ABB1-3B805CC76119');
              LSrc := ALTrim(AlCopyStr(LSrc, P2 + 3, maxint));
            end;
          end;

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

          LSrc := ALStringReplaceA(LSrc,'\'#13#10, ' ');
          LSrc := ALStringReplaceA(LSrc,' * const ',' * ');
          LSrc := ALStringReplaceA(LSrc,#13#10'_Check_return_'#13#10, #13#10);
          LSrc := ALStringReplaceA(LSrc,#13#10'_Must_inspect_result_'#13#10, #13#10);
          LSrc := ALStringReplaceA(LSrc,#13#10'EXTERN_C'#13#10, #13#10);
          LSrc := ALStringReplaceA(LSrc,#13#10' union'#13#10' {', #13#10' union {');
          LSrc := ALStringReplaceA(LSrc,#13#10' struct'#13#10' {', #13#10' struct {');
          LSrc := ALStringReplaceA(LSrc,#13#10'extern "C" {'#13#10, #13#10);
          LSrc := ALStringReplaceA(LSrc,#13#10'} // extern "C"'#13#10, #13#10);

          // typedef enum { MONGOC_SERVER_API_V1 } mongoc_server_api_version_t;
          // =>
          // typedef enum
          // {
          //   MONGOC_SERVER_API_V1
          // } mongoc_server_api_version_t;
          LLst := TalStringListA.Create;
          try
            LLst.Text := LSrc;
            For var I := LLst.Count - 1 downto 0 do begin
              var LLine := LLst[I];
              if (AlposA('typedef ', LLine) = 1) and (ALposA('{', LLine) > 0) then begin
                LLine := ALStringReplaceA(LLine, '{', #13#10'{'#13#10);
                LLine := ALStringReplaceA(LLine, '}', #13#10'}');
                LLst[I] := ALTrim(LLine);
              end;
            end;
            LSrc := ALTrim(LLst.Text);
          finally
            ALFreeAndNil(LLst);
          end;

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

          LSrc := RemoveSection(LSrc, '!_MSC_VER');
          LSrc := RemoveSection(LSrc, '!defined(_MSC_VER)');
          LSrc := RemoveSection(LSrc, '!__cplusplus');
          LSrc := RemoveSection(LSrc, '!defined(__cplusplus)');
          LSrc := RemoveSection(LSrc, '!defined(__cplusplus) && !defined(SORTPP_PASS)');
          LSrc := RemoveSection(LSrc, '!defined(__cplusplus) && (__cplusplus >= 201103L || defined(_MSVC_LANG))');
          LSrc := RemoveSection(LSrc, 'defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L');
          LSrc := RemoveSection(LSrc, 'defined(__STDC_VERSION__) && __STDC_VERSION__ >= 202311L');
          LSrc := RemoveSection(LSrc, '!defined (_MSC_VER)');
          LSrc := RemoveSection(LSrc, '!( _MSC_VER >= 800 )');
          LSrc := RemoveSection(LSrc, '!!defined(_MSC_VER) || (_MSC_VER >= 1800)');
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
          LSrc := RemoveSection(LSrc, 'defined(_WIN32_WINNT) && (_WIN32_WINNT < 0x0601)');
          LSrc := RemoveSection(LSrc, '!_WIN32_WINNT');

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
            LSrc := ALStringReplaceA(LSrc,' SEC_ENTRY'#13#10,' __CALLCONV__'#13#10);
            LSrc := ALStringReplaceA(LSrc,'(SEC_ENTRY * ','(__CALLCONV__ * ');
            LSrc := ALStringReplaceA(LSrc,'#define __SEC_FAR SEC_FAR','');
            LSrc := ALStringReplaceA(LSrc,' SEC_FAR * ',' * ');
          end;
          {$ENDREGION}

          {$REGION 'schannel.h'}
          if LChoice = 2 then begin
            LSrc := RemoveSection(LSrc, '__SCHANNEL_H__');
            LSrc := RemoveSection(LSrc, '!SCHANNEL_USE_BLACKLISTS');
            LSrc := ALStringReplaceA(LSrc,' WINAPI'#13#10,' __CALLCONV__'#13#10);
            LSrc := ALStringReplaceA(LSrc,'(WINAPI * ','(__CALLCONV__ * ');

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
            LSrc := ALStringReplaceA(LSrc,' HTTPAPI_LINKAGE'#13#10,' __CALLCONV__'#13#10);
            LSrc := ALStringReplaceA(LSrc,' WINAPI'#13#10,' __CALLCONV__'#13#10);
            LSrc := ALStringReplaceA(LSrc,'(WINAPI * ','(__CALLCONV__ * ');
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
            LSrc := ALStringReplaceA(LSrc,' WINAPI'#13#10,' __CALLCONV__'#13#10);
            LSrc := ALStringReplaceA(LSrc,'(WINAPI * ','(__CALLCONV__ * ');
          end;
          {$ENDREGION}

          {$REGION 'mongodb*.h'}
          if LChoice = 5 then begin
            LSrc := RemoveSection(LSrc, '__FILENAME__');
            LSrc := ALStringReplaceA(LSrc, '// #define __FILENAME__'#13#10, '');
            LSrc := RemoveSection(LSrc, 'BCON_H_');
            LSrc := ALStringReplaceA(LSrc, '// #define BCON_H_'#13#10, '');
            LSrc := RemoveSection(LSrc, 'BSON_BSON_T_H_INCLUDED');
            LSrc := ALStringReplaceA(LSrc, '// #define BSON_BSON_T_H_INCLUDED'#13#10, '');
            LSrc := RemoveSection(LSrc, 'BSON_COMPAT_H');
            LSrc := ALStringReplaceA(LSrc, '// #define BSON_COMPAT_H'#13#10, '');
            LSrc := RemoveSection(LSrc, 'BSON_CONFIG_H');
            LSrc := ALStringReplaceA(LSrc, '// #define BSON_CONFIG_H'#13#10, '');
            LSrc := RemoveSection(LSrc, 'BSON_ERROR_T_INCLUDED');
            LSrc := ALStringReplaceA(LSrc, '// #define BSON_ERROR_T_INCLUDED'#13#10, '');
            LSrc := RemoveSection(LSrc, 'BSON_MACROS_H');
            LSrc := ALStringReplaceA(LSrc, '// #define BSON_MACROS_H'#13#10, '');
            LSrc := RemoveSection(LSrc, 'BSON_MEMORY_H_INCLUDED');
            LSrc := ALStringReplaceA(LSrc, '// #define BSON_MEMORY_H_INCLUDED'#13#10, '');
            LSrc := RemoveSection(LSrc, 'BSON_VERSION_H');
            LSrc := ALStringReplaceA(LSrc, '// #define BSON_VERSION_H'#13#10, '');
            LSrc := RemoveSection(LSrc, 'MONGOC_ERRORS_H');
            LSrc := ALStringReplaceA(LSrc, '// #define MONGOC_ERRORS_H'#13#10, '');

            LSrc := RemoveSection(LSrc, '!_SSIZE_T_DEFINED');
            LSrc := RemoveSection(LSrc, 'defined(__clang__)');
            LSrc := RemoveSection(LSrc, 'defined(__clang__) && defined(__clang_major__) && defined(__clang_minor__) && (__clang_major__ >= 3) && (__clang_minor__ >= 1)');
            LSrc := RemoveSection(LSrc, 'defined(__GNUC__) || defined(__clang__)');
            LSrc := RemoveSection(LSrc, 'defined(__GNUC__) && (__GNUC__ >= 4)');
            LSrc := RemoveSection(LSrc, 'defined(__GNUC__) && 2 < __GNUC__ + (8 <= __GNUC_MINOR__)');
            LSrc := RemoveSection(LSrc, 'defined(__GNUC__)');
            LSrc := RemoveSection(LSrc, 'defined(__MINGW32__)');
            LSrc := RemoveSection(LSrc, 'defined(__MINGW32__) && !defined(INIT_ONCE_STATIC_INIT)');
            LSrc := RemoveSection(LSrc, '!BSON_UINT16_SWAP_LE_BE');
            LSrc := RemoveSection(LSrc, '!BSON_UINT32_SWAP_LE_BE');
            LSrc := RemoveSection(LSrc, '!BSON_UINT64_SWAP_LE_BE');
            LSrc := RemoveSection(LSrc, '!BSON_BYTE_ORDER == BSON_LITTLE_ENDIAN');
            LSrc := RemoveSection(LSrc, 'BSON_BYTE_ORDER == BSON_BIG_ENDIAN');
            LSrc := RemoveSection(LSrc, 'BSON_OS == 1');
            LSrc := RemoveSection(LSrc, '!BSON_OS == 2');
            LSrc := RemoveSection(LSrc, '!BSON_OS_WIN32');
            LSrc := RemoveSection(LSrc, '!defined(BSON_INSIDE) && !defined(BSON_COMPILATION)');
            LSrc := RemoveSection(LSrc, '!defined(MONGOC_INSIDE) && !defined(MONGOC_COMPILATION)');
            LSrc := RemoveSection(LSrc, 'defined(BSON_COMPILATION)');
            LSrc := RemoveSection(LSrc, '!NOMINMAX');
            LSrc := RemoveSection(LSrc, '!WIN32_LEAN_AND_MEAN');
            LSrc := RemoveSection(LSrc, 'BSON_OS_UNIX');
            LSrc := RemoveSection(LSrc, 'defined(BSON_OS_UNIX)');
            LSrc := RemoveSection(LSrc, 'PRIi32');
            LSrc := RemoveSection(LSrc, 'PRId32');
            LSrc := RemoveSection(LSrc, 'PRIu32');
            LSrc := RemoveSection(LSrc, 'PRIi64');
            LSrc := RemoveSection(LSrc, 'PRId64');
            LSrc := RemoveSection(LSrc, 'PRIu64');
            LSrc := RemoveSection(LSrc, 'SSIZE_MAX');
            LSrc := RemoveSection(LSrc, 'SSIZE_MIN');
            LSrc := RemoveSection(LSrc, 'defined(va_copy) && defined(__va_copy)');
            LSrc := RemoveSection(LSrc, 'defined(va_copy)');
            LSrc := RemoveSection(LSrc, '!BSON_HAVE_STDBOOL_H != 1');
            LSrc := RemoveSection(LSrc, '!BSON_HAVE_CLOCK_GETTIME != 1');
            LSrc := RemoveSection(LSrc, '!BSON_HAVE_STRINGS_H != 1');
            LSrc := RemoveSection(LSrc, '!BSON_HAVE_STRNLEN != 1');
            LSrc := RemoveSection(LSrc, '!BSON_HAVE_SNPRINTF != 1');
            LSrc := RemoveSection(LSrc, '!BSON_HAVE_GMTIME_R != 1');
            LSrc := RemoveSection(LSrc, '!BSON_HAVE_TIMESPEC != 1');
            LSrc := RemoveSection(LSrc, '!BSON_HAVE_RAND_R != 1');
            LSrc := RemoveSection(LSrc, '!BSON_HAVE_STRLCPY != 1');
            LSrc := RemoveSection(LSrc, '!BSON_HAVE_ALIGNED_ALLOC != 1');
            LSrc := RemoveSection(LSrc, 'BSON_STATIC');
            LSrc := RemoveSection(LSrc, 'MIN');
            LSrc := RemoveSection(LSrc, 'MAX');
            LSrc := RemoveSection(LSrc, 'ABS');
            LSrc := RemoveSection(LSrc, 'BSON_GNUC_CHECK_VERSION(4, 0) && !defined(_WIN32)');
            LSrc := RemoveSection(LSrc, 'BSON_GNUC_CHECK_VERSION(4, 4)');
            LSrc := RemoveSection(LSrc, 'defined(__GNUC__) && (defined(__clang__) || BSON_GNUC_CHECK_VERSION(4, 5))');
            LSrc := RemoveSection(LSrc, '!MONGOC_ENABLE_SSL');
            LSrc := RemoveSection(LSrc, 'MONGOC_DEFAULT_CONNECTTIMEOUTMS');
            LSrc := RemoveSection(LSrc, 'MONGOC_DEFAULT_SOCKETTIMEOUTMS');
            LSrc := RemoveSection(LSrc, 'MONGOC_ENABLE_SSL_SECURE_CHANNEL != 1');
            LSrc := RemoveSection(LSrc, 'MONGOC_ENABLE_CRYPTO_CNG != 1');
            LSrc := RemoveSection(LSrc, 'MONGOC_HAVE_BCRYPT_PBKDF2 != 1');
            LSrc := RemoveSection(LSrc, '!MONGOC_ENABLE_SSL_SECURE_TRANSPORT != 1');
            LSrc := RemoveSection(LSrc, '!MONGOC_ENABLE_CRYPTO_COMMON_CRYPTO != 1');
            LSrc := RemoveSection(LSrc, '!MONGOC_ENABLE_SSL_OPENSSL != 1');
            LSrc := RemoveSection(LSrc, '!MONGOC_ENABLE_SSL_OPENSSL');
            LSrc := RemoveSection(LSrc, '!MONGOC_ENABLE_CRYPTO_LIBCRYPTO != 1');
            LSrc := RemoveSection(LSrc, 'MONGOC_ENABLE_SSL != 1');
            LSrc := RemoveSection(LSrc, 'MONGOC_ENABLE_CRYPTO != 1');
            LSrc := RemoveSection(LSrc, '!MONGOC_ENABLE_CRYPTO_SYSTEM_PROFILE != 1');
            LSrc := RemoveSection(LSrc, '!MONGOC_HAVE_ASN1_STRING_GET0_DATA != 1');
            LSrc := RemoveSection(LSrc, 'MONGOC_ENABLE_SASL != 1');
            LSrc := RemoveSection(LSrc, '!MONGOC_ENABLE_SASL_CYRUS != 1');
            LSrc := RemoveSection(LSrc, 'MONGOC_ENABLE_SASL_SSPI != 1');
            LSrc := RemoveSection(LSrc, '!MONGOC_HAVE_SASL_CLIENT_DONE != 1');
            LSrc := RemoveSection(LSrc, 'MONGOC_HAVE_SOCKLEN != 1');
            LSrc := RemoveSection(LSrc, 'MONGOC_HAVE_DNSAPI != 1');
            LSrc := RemoveSection(LSrc, '!MONGOC_HAVE_RES_NSEARCH != 1');
            LSrc := RemoveSection(LSrc, '!MONGOC_HAVE_RES_NDESTROY != 1');
            LSrc := RemoveSection(LSrc, '!MONGOC_HAVE_RES_NCLOSE != 1');
            LSrc := RemoveSection(LSrc, '!MONGOC_HAVE_RES_SEARCH != 1');
            LSrc := RemoveSection(LSrc, 'MONGOC_ENABLE_COMPRESSION != 1');
            LSrc := RemoveSection(LSrc, '!MONGOC_ENABLE_COMPRESSION_SNAPPY != 1');
            LSrc := RemoveSection(LSrc, 'MONGOC_ENABLE_COMPRESSION_ZLIB != 1');
            LSrc := RemoveSection(LSrc, '!MONGOC_ENABLE_COMPRESSION_ZSTD != 1');
            LSrc := RemoveSection(LSrc, '!MONGOC_ENABLE_SHM_COUNTERS != 1');
            LSrc := RemoveSection(LSrc, '!MONGOC_ENABLE_RDTSCP != 1');
            LSrc := RemoveSection(LSrc, '!MONGOC_HAVE_SCHED_GETCPU != 1');
            LSrc := RemoveSection(LSrc, '!MONGOC_ENABLE_CLIENT_SIDE_ENCRYPTION != 1');
            LSrc := RemoveSection(LSrc, '!MONGOC_HAVE_SS_FAMILY != 1');
            LSrc := RemoveSection(LSrc, 'MONGOC_ENABLE_MONGODB_AWS_AUTH != 1');
            LSrc := RemoveSection(LSrc, '_POSIX_HOST_NAME_MAX');
            LSrc := RemoveSection(LSrc, 'MONGOC_LOG_DOMAIN');
            LSrc := RemoveSection(LSrc, 'MONGOC_STATIC');
            LSrc := RemoveSection(LSrc, 'defined(MONGOC_COMPILATION)');
            LSrc := RemoveSection(LSrc, 'defined(_AIX) && !defined(MONGOC_HAVE_SS_FAMILY)');
            LSrc := RemoveSection(LSrc, 'MONGOC_DEFAULT_PORT');

            LSrc := ReplaceSection(LSrc, '_WIN32', 'defined(Win32)');
            LSrc := ReplaceSection(LSrc, '_WIN64', 'defined(Win64)');
            LSrc := ReplaceSection(LSrc, 'defined(__LP64__) || defined(_LP64)', 'defined(CPUX64) or defined(CPUARM64)');
            LSrc := ReplaceSection(LSrc, 'BSON_WORD_SIZE == 64', 'defined(CPUX64) or defined(CPUARM64)');

            LSrc := ALStringReplaceA(LSrc,#13#10'// BSON_BEGIN_DECLS'#13#10,#13#10);
            LSrc := ALStringReplaceA(LSrc,#13#10'// BSON_END_DECLS'#13#10,#13#10);
            LSrc := ALStringReplaceA(LSrc,'// #define BSON_API'#13#10,'');
            LSrc := ALStringReplaceA(LSrc,'// #define BSON_CALL'#13#10,'');
            LSrc := ALStringReplaceA(LSrc,'// #define BSON_EXPORT(type) BSON_API type BSON_CALL'#13#10,'');
            LSrc := ALStringReplaceA(LSrc,' BSON_CALL'#13#10,' __CALLCONV__'#13#10);
            LSrc := ALStringReplaceA(LSrc,'(BSON_CALL * ','(__CALLCONV__ * ');
            LSrc := ALStringReplaceA(LSrc,'// #define MONGOC_API'#13#10,'');
            LSrc := ALStringReplaceA(LSrc,'// #define MONGOC_CALL'#13#10,'');
            LSrc := ALStringReplaceA(LSrc,'// #define MONGOC_EXPORT(type) MONGOC_API type MONGOC_CALL'#13#10,'');
            LSrc := ALStringReplaceA(LSrc,' MONGOC_CALL'#13#10,' __CALLCONV__'#13#10);
            LSrc := ALStringReplaceA(LSrc,'(MONGOC_CALL * ','(__CALLCONV__ * ');
            LSrc := ALStringReplaceA(LSrc,'(1U << 31)','(NativeUInt(1) << 31)');
            LSrc := ALStringReplaceA(LSrc,'(10 * 1000L)','(10 * 1000)');
            LSrc := ALStringReplaceA(LSrc,'(1000L * 60L * 5L)','(1000 * 60 * 5)');
            LSrc := ALStringReplaceA(LSrc,'#define BSON_PRERELEASE_VERSION (','// #define BSON_PRERELEASE_VERSION (');
            LSrc := ALStringReplaceA(LSrc,'#define BSON_VERSION (','// #define BSON_VERSION (');
            LSrc := ALStringReplaceA(LSrc,'#define MONGOC_PRERELEASE_VERSION (','// #define MONGOC_PRERELEASE_VERSION (');
            LSrc := ALStringReplaceA(LSrc,'#define MONGOC_VERSION (','// #define MONGOC_VERSION (');
            LSrc := ALStringReplaceA(LSrc,'#define MONGOC_SOCKET_ARG3 socklen_t','typedef socklen_t MONGOC_SOCKET_ARG3;');
            LSrc := ALStringReplaceA(LSrc,'// (* * < private > * *)','// // private');
            LSrc := ALStringReplaceA(LSrc,') BSON_GNUC_WARN_UNUSED_RESULT;',');');
            LSrc := ALStringReplaceA(LSrc,')'#13#10'// BSON_GNUC_WARN_UNUSED_RESULT;',');');
            LSrc := ALStringReplaceA(LSrc,') BSON_GNUC_NULL_TERMINATED;',');');
            LSrc := ALStringReplaceA(LSrc,')'#13#10'// BSON_GNUC_NULL_TERMINATED;',');');
            LSrc := ALStringReplaceA(LSrc,') BSON_GNUC_PURE;',');');
            LSrc := ALStringReplaceA(LSrc,')'#13#10'// BSON_GNUC_PURE;',');');
            LSrc := ALStringReplaceA(LSrc,' (* IGNORED *),',', // IGNORED');
            LSrc := ALStringReplaceA(LSrc,' (* May be NULL *),',', // May be NULL');
            LSrc := ALStringReplaceA(LSrc,') BSON_GNUC_PRINTF(1, 0);',');');
            LSrc := ALStringReplaceA(LSrc,') BSON_GNUC_PRINTF(1, 2);',');');
            LSrc := ALStringReplaceA(LSrc,') BSON_GNUC_PRINTF(3, 0);',');');
            LSrc := ALStringReplaceA(LSrc,') BSON_GNUC_PRINTF(3, 4);',');');
            LSrc := ALStringReplaceA(LSrc,') BSON_GNUC_PRINTF(4, 5);',');');
            LSrc := ALStringReplaceA(LSrc,', char utf8[6],',', TCharArray6 utf8,');
            LSrc := ALStringReplaceA(LSrc,', char str[25])',', TCharArray25 str)');
            LSrc := ALStringReplaceA(LSrc,'// #define BSON_BEGIN_DECLS extern "C" {','');
            LSrc := ALStringReplaceA(LSrc,'// #define BSON_END_DECLS }','');
            LSrc := ALStringReplaceA(LSrc,'mongoc_change_stream_destroy(mongoc_change_stream_t * );','mongoc_change_stream_destroy(mongoc_change_stream_t * stream);');
            LSrc := ALStringReplaceA(LSrc,'mongoc_change_stream_get_resume_token(mongoc_change_stream_t * );','mongoc_change_stream_get_resume_token(mongoc_change_stream_t * stream);');
            LSrc := ALStringReplaceA(LSrc,'mongoc_change_stream_next(mongoc_change_stream_t * , const bson_t * * );','mongoc_change_stream_next(mongoc_change_stream_t * stream, const bson_t * * bson);');
            LSrc := ALStringReplaceA(LSrc,'mongoc_change_stream_error_document(const mongoc_change_stream_t * , bson_error_t * , const bson_t * * );','mongoc_change_stream_error_document(const mongoc_change_stream_t * stream, bson_error_t * err, const bson_t * * bson);');
            LSrc := ALStringReplaceA(LSrc,'mongoc_client_encryption_encrypt_text_prefix_opts_destroy(mongoc_client_encryption_encrypt_text_prefix_opts_t * );','mongoc_client_encryption_encrypt_text_prefix_opts_destroy(mongoc_client_encryption_encrypt_text_prefix_opts_t * opts);');
            LSrc := ALStringReplaceA(LSrc,'mongoc_client_encryption_encrypt_text_suffix_opts_destroy(mongoc_client_encryption_encrypt_text_suffix_opts_t * );','mongoc_client_encryption_encrypt_text_suffix_opts_destroy(mongoc_client_encryption_encrypt_text_suffix_opts_t * opts);');
            LSrc := ALStringReplaceA(LSrc,'mongoc_client_encryption_encrypt_text_substring_opts_destroy(mongoc_client_encryption_encrypt_text_substring_opts_t * );','mongoc_client_encryption_encrypt_text_substring_opts_destroy(mongoc_client_encryption_encrypt_text_substring_opts_t * opts);');
            LSrc := ALStringReplaceA(LSrc,'mongoc_find_and_modify_opts_set_flags(mongoc_find_and_modify_opts_t * opts, const mongoc_find_and_modify_flags_t flags);','mongoc_find_and_modify_opts_set_flags(mongoc_find_and_modify_opts_t * opts, const UInt32 flags);');

            LSrc := ALStringReplaceA(
                      LSrc,
                      '{$if defined(Win32)}'#13#10+
                      '// #include <stddef.h>'#13#10+
                      '{$else}'#13#10+
                      '// #include <sys/uio.h>'#13#10+
                      '{$endif}',
                      '');
            LSrc := ALStringReplaceA(
                      LSrc,
                      '// typedef struct _mongoc_client_encryption_rewrap_many_datakey_result_t'#13#10+
                      '// mongoc_client_encryption_rewrap_many_datakey_result_t;',
                      '// typedef struct _mongoc_client_encryption_rewrap_many_datakey_result_t mongoc_client_encryption_rewrap_many_datakey_result_t;');
            LSrc := ALStringReplaceA(LSrc, 'BSON_ALIGNED_BEGIN(BSON_ALIGN_OF_PTR) ', '');
            LSrc := ALStringReplaceA(LSrc, 'BSON_ALIGNED_BEGIN(BSON_ALIGN_OF_PTR)', '');
            LSrc := ALStringReplaceA(LSrc, ' BSON_ALIGNED_END(BSON_ALIGN_OF_PTR);', ';');

            LSrc := ALStringReplaceA(LSrc, 'BSON_EXPORT(const ', 'BSON_EXPORT(');
            P1 := AlPosA('BSON_EXPORT(', LSrc);
            While P1 > 0 do begin
              Delete(LSrc, P1, length('BSON_EXPORT('));
              var P2 := AlposA(')', LSrc, P1);
              If P2 <= 0 then
                raise Exception.Create('Error 6FB459CF-9953-4F53-B174-D21C84512B71');
              Delete(LSrc, P2, length(')'));
              insert(' __CALLCONV__ ', LSrc, P2);
              P1 := AlPosA('BSON_EXPORT(', LSrc);
            end;

            LSrc := ALStringReplaceA(LSrc, 'MONGOC_EXPORT(const ', 'MONGOC_EXPORT(');
            P1 := AlPosA('MONGOC_EXPORT(', LSrc);
            While P1 > 0 do begin
              Delete(LSrc, P1, length('MONGOC_EXPORT('));
              var P2 := AlposA(')', LSrc, P1);
              If P2 <= 0 then
                raise Exception.Create('Error 6FB459CF-9953-4F53-B174-D21C84512B71');
              Delete(LSrc, P2, length(')'));
              insert(' __CALLCONV__ ', LSrc, P2);
              P1 := AlPosA('MONGOC_EXPORT(', LSrc);
            end;

          end;
          {$ENDREGION}

          {$REGION 'wincrypt.h'}
          //LSrc := ALStringReplaceA(LSrc,' WINADVAPI'#13#10,' __CALLCONV__'#13#10);
          //LSrc := ALStringReplaceA(LSrc,' WINCRYPT32API'#13#10,' __CALLCONV__'#13#10);
          //LSrc := ALStringReplaceA(LSrc,' WINCRYPT32STRINGAPI'#13#10,' __CALLCONV__'#13#10);
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
          //LSrc := ALStringReplaceA(LSrc,'(CALLBACK * ','(__CALLCONV__ * ');
          {$ENDREGION}

          while alposA('  ', LSrc) > 0 do
            LSrc := ALStringReplaceA(LSrc,'  ', ' ');

          {$IF defined(debug)}
          ALSaveStringTofile(LSrc, ALGetModulePathW + 'debug_src.txt');
          {$ENDIF}

          LSrc := ConvertDefine(LSrc);
          LSrc := ConvertEnums(LSrc, LCallingConvention, LDelayed);
          LSrc := ConvertSimpleType(LSrc);
          LSrc := ConvertRecords(LSrc, LCallingConvention, LDelayed);

          LSrc := ALStringReplaceA(LSrc,#13#10'// (__CALLCONV__ * ',#13#10'// (#1 * ');
          LSrc := ALStringReplaceA(LSrc,'(__CALLCONV__ * ',#13#10'// (__CALLCONV__ * ');
          LSrc := ALStringReplaceA(LSrc,#13#10'// (#1 * ',#13#10'// (__CALLCONV__ * ');

          LSrc := Convertfunctions(LSrc, LCallingConvention, LDelayed, LLibraryDLLEntryPoints);

          LSrc := AlStringReplaceA(LSrc, #13#10'// (*', #13#10'// *');
          LSrc := AlStringReplaceA(LSrc, #13#10'// *)', #13#10'// *');

          LSrc := '''
            unit
            ''' +
            ' ' + ALExtractFileName(AnsiString(LDstfileName), true{RemoveFileExt}) + ';' + #13#10 +
            '''

            //*******************************************************************//
            // This unit was automatically generated by CHeaderWrapperGenerator. //
            // Do not edit manually.                                             //
            //*******************************************************************//

            interface

            {$I Alcinoe.inc}

            {$WARN SYMBOL_PLATFORM OFF}
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
            #13#10 +
            MakeLibraryInterface(LLibraryClassName, LLibraryDLLEntryPoints)+#13#10 +
            #13#10 +
            'implementation'#13#10 +
            #13#10 +
            MakeLibraryImplementation(LLibraryClassName, LLibraryDLLEntryPoints, LDstfileName)+#13#10 +
            'end.';


          {$REGION 'mongodb*.h'}
          if LChoice = 5 then begin
            LSrc := ALStringReplaceA(LSrc,'__LibraryConstructor__', 'mongoc_init;');
            LSrc := ALStringReplaceA(LSrc,'__LibraryDestructor__','mongoc_cleanup;');
          end;
          {$ENDREGION}

          LSrc := ALStringReplaceA(LSrc,'__LibraryConstructor__', '');
          LSrc := ALStringReplaceA(LSrc,'__LibraryDestructor__','');

          LLst := TALStringListA.Create;
          try
            LLst.Text := LSrc;
            For var i := LLst.Count - 1 downto 0 do begin
              LLst[I] := ALTrimRight(LLst[I]);
              if ALTrim(LLst[I]) = '//' then LLst[I] := ''
              else if ALPosA('// #pragma ', ALTrim(LLst[I])) = 1 then LLst[I] := ''
              else if ALPosA('// #include ', ALTrim(LLst[I])) = 1 then LLst[I] := ''
              else if ALPosA('// #undef ', ALTrim(LLst[I])) = 1 then LLst[I] := ''
              else if (I > 0) and (AlposA('): ', ALTrim(LLst[I])) = 1) then begin
                var LStr := LLst[I-1];
                var LComment: AnsiString := '';
                P1 := AlposA(' //', LStr, 4);
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
                P1 := AlposA(' //', LStr, 4);
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

          LLst := TALStringListA.Create;
          try
            LLst.Text := LSrc;
            For var i := LLst.Count - 1 downto 0 do begin
              LLst[I] := ALTrimRight(LLst[I]);
              if (alposA('// ', LLst[I]) = 1) and (alposA(' *)', LLst[I]) = length(LLst[I]) - 2) then
                LLst[I] := ALcopyStr(LLst[I], 1, length(LLst[I]) - 3);
            end;
            LSrc := ALTrim(LLst.Text);
          finally
            AlFreeAndNil(LLst);
          end;

          LLst := TALStringListA.Create;
          try
            LLst.Text := LSrc;
            For var i := LLst.Count - 1 downto 0 do
              if (alposA('//[', LLst[I]) = 1) then
                LLst[I] := ALStringReplaceA(LLst[I], '//[', '// [', []);
            LSrc := ALTrim(LLst.Text);
          finally
            AlFreeAndNil(LLst);
          end;

          while ALposA(#13#10#13#10#13#10, LSrc) > 0 do
            LSrc := ALStringReplaceA(LSrc,#13#10#13#10#13#10,#13#10#13#10,[RfReplaceALL]);

          LSrc := ALStringReplaceA(
                    LSrc,
                    '{$if defined(Win32)}'#13#10+
                    #13#10+
                    '{$else}'#13#10+
                    #13#10+
                    '{$endif}'#13#10,
                    '');

          LSrc := ALStringReplaceA(
                    LSrc,
                    '{$if defined(Win64)}'#13#10+
                    #13#10+
                    '{$else}'#13#10+
                    #13#10+
                    '{$endif}'#13#10,
                    '');

          while ALposA(#13#10#13#10#13#10, LSrc) > 0 do
            LSrc := ALStringReplaceA(LSrc,#13#10#13#10#13#10,#13#10#13#10,[RfReplaceALL]);

          ALSaveStringToFile(LSrc, LDstfileName);

        finally
          ALFreeAndNil(LLibraryDLLEntryPoints);
        end;

      end;

      Writeln('');
      Writeln('Build successful');
      Writeln('Press <Enter> key to quit');
      Readln;

    finally
      for var I := Low(CandidateDllModules) to High(CandidateDllModules) do
        FreeLibrary(CandidateDllModules[I]);
    end;

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