program ConvertToDelphi;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  system.AnsiStrings,
  Alcinoe.StringList,
  Alcinoe.StringUtils,
  System.SysUtils;

begin
  try
     var LLst := TALStringListA.Create;
     var LLst2 := TALStringListA.Create;
     LLst2.LineBreak := ', *';
     var LLst3 := TALStringListA.Create;
     LLst3.LineBreak := ' ';
     var LLst4 := TALStringListA.Create;
     LLst4.LineBreak := ' * ';
     var LSrc: AnsiString := ALGetStringFromFile('c:\Dev\MagicFoundation\Alcinoe\References\http.sys\http.h');

      LSrc := ALStringReplaceA(LSrc,'#ifndef __SECSTATUS_DEFINED__'#13#10,'',[RfReplaceALL]);
      LSrc := ALStringReplaceA(LSrc,'#define __SECSTATUS_DEFINED__'#13#10,'',[RfReplaceALL]);
      LSrc := ALStringReplaceA(LSrc,'#define HTTPAPI_LINKAGE DECLSPEC_IMPORT'#13#10,'',[RfReplaceALL]);
      LSrc := ALStringReplaceA(LSrc,' enum  ',' enum ',[RfReplaceALL]);
      LSrc := ALStringReplaceA(LSrc,'typedef enum _HTTP_URI_SCHEME','typedef enum _HTTP_SCHEME',[RfReplaceALL]);
      LSrc := ALStringReplaceA(LSrc,','#13#10' *',', *',[RfReplaceALL]);
      LSrc := ALStringReplaceA(LSrc,'ULONG Present:1;','ULONG Present;',[RfReplaceALL]);

     LLst.Text := LSrc;
      For Var i := LLst.Count - 1 downto 0 do
        if ALposA('*', ALTrim(LLst[I])) = 1 then begin
          LLst[I-1] := LLst[I-1] + ' ' + ALTrim(LLst[I]);
          LLst.Delete(i);
        end;
      LSrc := ALTrim(LLst.Text);

     LSrc := '//' + ALStringReplaceA(LSrc,#13#10,#13#10'// ',[RfReplaceALL]);
     LSrc := '''
             unit Alcinoe.Winapi.HttpApi;

             interface

             {$MINENUMSIZE 4}
             {$A+}

             uses
               WinApi.Windows,
               Winapi.Winsock2;

             //
             // Type aliases
             //

             type
               PSTR = LPSTR;
               PWSTR = LPWSTR;
               PCSTR = LPCSTR;
               PCWSTR = LPCWSTR;
               HANDLE = THandle;
               GUID = TGUID;

             ''' +
             '' + LSrc + #13#10 +
             '''

             implementation

             end.
             ''';

      // // typedef enum _
      var P1 := alposA('// typedef enum _',LSrc);
      while P1 > 0 do begin
        var P2 := alposA('// }',LSrc, P1 + 1);
        if P2 <= 0 then raise Exception.Create('Error 0DCAB0CB-D0C3-42E9-9395-1080847A491D');
        LLst.Text := AlCopyStr(LSrc, P1, P2-P1);
        if LLst.Count < 2 then raise Exception.Create('Error Message');
        if Llst[1] <> '// {' then raise Exception.Create('Error BCDE5C8F-53CE-4F29-B687-1B3F3D6FF249');
        LLst.Delete(1);
        LLst[0] := ALStringReplaceA(LLst[0], '// typedef enum _', '', []);
        LLst[0] := LLst[0] + ' = (';
        LLst[0] := '  ' + ALTrim(LLst[0]);
        For Var i := 1 to LLst.Count - 1 do
          LLst[I] := ALTrim(ALcopyStr(LLst[I], 4, maxint));
        For Var i := LLst.Count - 1 downto 0 do
          if LLst[I] = '' then LLst.Delete(i);
        For Var i := LLst.Count - 1 downto 0 do
          if LLst[I] = '//' then LLst.Delete(i);
        For Var i := LLst.Count - 1 downto 0 do
          if LLst[I] = '#if _WIN32_WINNT >= _WIN32_WINNT_WIN8' then LLst.Delete(i);
        For Var i := LLst.Count - 1 downto 0 do
          if LLst[I] = '#if _WIN32_WINNT >= _WIN32_WINNT_WIN10' then LLst.Delete(i);
        For Var i := LLst.Count - 1 downto 0 do
          if LLst[I] = '#endif' then LLst.Delete(i);
        For Var i := LLst.Count - 1 downto 1 do
          LLst[I] := '    ' + ALTrim(LLst[I]);
        LLst[Llst.count - 1] := ALStringReplaceA(LLst[Llst.count - 1], ',', '',[]);
        LLst.Insert(0, 'type');
        delete(LSrc, P1, P2-P1);
        var P3 := ALPosA(#13#10,LSrc, P1);
        var Lstr := ALCopyStr(LSrc,P1,P3-P1);
        if alposA('// }', Lstr) <> 1 then
          Raise Exception.Create('Error');
        delete(Lstr, 1, 4);
        if alposA('*', Lstr) > 0 then begin
          LLst2.Text := ALTrim(AlstringReplaceA(LStr,';','',[RfReplaceALL]));
          if LLst2.Count <> 2 then
            raise Exception.Create('Error Message');
          Lstr := ALTrimright(LLst.Text)+');'+#13#10 +
                  '  '+LLst2[1] + ' = ^' + LLst2[0] +';'#13#10;
        end
        else
          Lstr := ALTrimright(LLst.Text)+');'+#13#10;
        delete(LSrc,P1, P3-P1);
        insert(Lstr,LSrc,P1);
        P1 := alposA('// typedef enum _',LSrc, P1+1);
      end;
     LSrc := ALStringReplaceA(LSrc,'HttpFeaturemax              = 0xFFFFFFFF);','HttpFeaturemax              = -1{ 0xFFFFFFFF });',[RfReplaceALL]);




      // // typedef enum _
      P1 := alposA('// typedef struct _',LSrc);
      while P1 > 0 do begin
        var P2 := alposA('// }',LSrc, P1 + 1);
        if P2 <= 0 then raise Exception.Create('Error 0DCAB0CB-D0C3-42E9-9395-1080847A491D');
        LLst.Text := AlCopyStr(LSrc, P1, P2-P1);
        if alposA('union', AlCopyStr(LSrc, P1, P2-P1)) > 0 then begin
          P1 := alposA('// typedef struct _',LSrc, P1+1);
          continue;
        end;
        if LLst.Count < 2 then raise Exception.Create('Error Message');
        if Llst[1] <> '// {' then raise Exception.Create('Error BCDE5C8F-53CE-4F29-B687-1B3F3D6FF249');
        LLst.Delete(1);
        LLst[0] := ALStringReplaceA(LLst[0], '// typedef struct _', '', []);
        LLst[0] := LLst[0] + ' = record';
        LLst[0] := '  ' + ALTrim(LLst[0]);
        For Var i := 1 to LLst.Count - 1 do
          LLst[I] := ALTrim(ALcopyStr(LLst[I], 4, maxint));
        For Var i := LLst.Count - 1 downto 0 do
          if LLst[I] = '' then LLst.Delete(i);
        For Var i := LLst.Count - 1 downto 0 do
          if LLst[I] = '//' then LLst.Delete(i);
        For Var i := LLst.Count - 1 downto 1 do begin
          var LStr := ALTrim(LLst[I]);
          if ALPosA('//', LStr) = 1 then continue;
          LStr := ALStringReplaceA(LStr, ' + ', '+', [rfReplaceALL]);
          While AlposA('  ', LStr) > 0 do
            LStr := ALStringReplaceA(LStr, '  ', ' ', [rfReplaceALL]);
          var P3 := AlposA(';', LStr);
          if P3 <= 0 then raise Exception.Create('Error Message');
          var LStr2 := ALCopyStr(LStr, P3, MaxInt);
          Lstr := ALCopyStr(Lstr, 1, P3-1);
          if ALposA(' * ', LStr) > 0 then begin
            LLst4.Text := LStr;
            if LLst4.count <> 2 then
              raise Exception.Create('Error Message');
            LLst[I] := LLst4[1] + ': ^' + LLst4[0] + Lstr2;
          end
          else begin
            LLst3.Text := LStr;
            if LLst3.count <> 2 then
              raise Exception.Create('Error Message');
            if ALPosA('[', LStr) > 0 then begin
              LLst[I] := LLst3[1] + ' of ' + LLst3[0] + Lstr2;
              LLst[I] := ALStringReplaceA(LLst[I], '[', ': array[0..', [rfReplaceALL]);
              LLst[I] := ALStringReplaceA(LLst[I], ']', '-1]', [rfReplaceALL]);
            end
            else begin
              LLst[I] := LLst3[1] + ': ' + LLst3[0] + Lstr2;
            end;
          end;
        end;
        For Var i := LLst.Count - 1 downto 1 do
          LLst[I] := '    ' + ALTrim(LLst[I]);
        LLst.Insert(0, 'type');
        delete(LSrc, P1, P2-P1);
        var P3 := ALPosA(#13#10,LSrc, P1);
        var Lstr := ALCopyStr(LSrc,P1,P3-P1);
        if alposA('// }', Lstr) <> 1 then
          Raise Exception.Create('Error');
        delete(Lstr, 1, 4);
        if alposA('*', Lstr) > 0 then begin
          LLst2.Text := ALTrim(AlstringReplaceA(LStr,';','',[RfReplaceALL]));
          if LLst2.Count <> 2 then
            raise Exception.Create('Error Message');
          Lstr := ALTrimright(LLst.Text)+#13#10'  end;'+#13#10 +
                  '  '+LLst2[1] + ' = ^' + LLst2[0] +';'#13#10;
        end
        else
          Lstr := ALTrimright(LLst.Text)+#13#10'  end;'+#13#10;
        delete(LSrc,P1, P3-P1);
        insert(Lstr,LSrc,P1);
        P1 := alposA('// typedef struct _',LSrc, P1+1);
      end;





      // // typedef USHORT HTTP_SERVICE_CONFIG_TIMEOUT_PARAM, *PHTTP_SERVICE_CONFIG_TIMEOUT_PARAM;
      P1 := alposA('// typedef ',LSrc);
      while P1 > 0 do begin
        var P2 := alposA(#13#10,LSrc, P1 + 1);
        if P2 <= 0 then raise Exception.Create('Error 0DCAB0CB-D0C3-42E9-9395-1080847A491D');
        var LStr := ALCopyStr(LSrc, P1, P2-P1);
        LStr := ALStringReplaceA(LStr, '// typedef ', '', []);
        While AlposA('  ', Lstr) > 0 do
          Lstr := ALStringReplaceA(Lstr,'  ',' ',[RfReplaceALL]);
        If AlposA(', *', Lstr) <= 0 then begin
          P1 := alposA('// typedef ',LSrc, P1+1);
          Continue;
        end;
        LStr := ALStringReplaceA(LStr, ', *', ' ', []);
        LStr := ALStringReplaceA(LStr, ';', '', []);
        LLst3.Text := Lstr;
        If LLst3.Count <> 3 then
          Raise Exception.Create('error');
        Lstr := 'type'+#13#10+
                '  ' + LLst3[1] + ' = ' + LLst3[0]+ ';' + #13#10 +
                '  ' + LLst3[2] + ' = ^' + LLst3[1]+ ';';
        delete(LSrc, P1, P2-P1);
        insert(Lstr,LSrc,P1);
        P1 := alposA('// typedef ',LSrc, P1+1);
      end;





      LSrc := ALStringReplaceA(LSrc,'// #define HTTP_MIN_ALLOWED_LOG_FILE_ROLLOVER_SIZE ((ULONG)(1 * 1024 * 1024))', 'const HTTP_MIN_ALLOWED_LOG_FILE_ROLLOVER_SIZE: ULONG = 1 * 1024 * 1024;',[]);
      LSrc := ALStringReplaceA(LSrc,'// #define HTTP_NULL_ID            (0ui64)', 'const HTTP_NULL_ID: UInt64 = 0;',[]);

      P1 := alposA('// #define ',LSrc);
      while P1 > 0 do begin
        var P2 := alposA(#13#10,LSrc, P1 + 1);
        if P2 <= 0 then raise Exception.Create('Error 0DCAB0CB-D0C3-42E9-9395-1080847A491D');
        var LStr := ALcopyStr(LSrc, P1, P2-P1); // // #define HTTP_AUTH_EX_FLAG_ENABLE_KERBEROS_CREDENTIAL_CACHING  (0x01)
        if AlposA('/', LStr, 3) > 0 then begin
          P1 := alposA('// #define ',LSrc, P1+1);
          continue;
        end;
        if AlposA('\', LStr, 3) > 0 then begin
          P1 := alposA('// #define ',LSrc, P1+1);
          continue;
        end;
        if AlposA('{', LStr, 3) > 0 then begin
          P1 := alposA('// #define ',LSrc, P1+1);
          continue;
        end;
        if AlposA('*', LStr, 3) > 0 then begin
          P1 := alposA('// #define ',LSrc, P1+1);
          continue;
        end;
        LStr := ALStringReplaceA(LStr, '// #define ', 'const ', []);
        LStr := AlTrim(Lstr);
        delete(LSrc, P1, P2-P1);
        P2 := High(Lstr);
        While Lstr[P2] <> ' ' do dec(P2);
        if P2 <= low(Lstr) then raise Exception.Create('Error Message');
        While Lstr[P2] = ' ' do dec(P2);
        if P2 <= low(Lstr) then raise Exception.Create('Error Message');
        inc(P2);
        insert(' = ', Lstr, P2);
        Lstr := Lstr + ';';
        While AlposA('  ', Lstr) > 0 do
          Lstr := ALStringReplaceA(Lstr,'  ',' ',[RfReplaceALL]);
        insert(Lstr, LSrc, P1);
        P1 := alposA('// #define ',LSrc, P1+1);
      end;
      LSrc := ALStringReplaceA(LSrc,'const = __HTTP_H__','// #define __HTTP_H__',[RfReplaceALL]);
     LSrc := ALStringReplaceA(LSrc,'0x','$',[RfReplaceALL]);
     LSrc := ALStringReplaceA(LSrc,'((ULONG)1024)',': ULONG = 1024',[RfReplaceALL]);
     LSrc := ALStringReplaceA(LSrc,'((ULONG)-1)',': ULONG = ULONG(-1)',[RfReplaceALL]);
     LSrc := ALStringReplaceA(LSrc,'((ULONGLONG)-1)',': ULONGLONG = ULONGLONG(-1)',[RfReplaceALL]);
     LSrc := ALStringReplaceA(LSrc,' = : ',': ',[RfReplaceALL]);


     LSrc := ALStringReplaceA(LSrc,' Type: ',' &Type: ',[RfReplaceALL]);
     LSrc := ALStringReplaceA(LSrc,'// typedef LONG SECURITY_STATUS;','type'#13#10'  SECURITY_STATUS = LONG;',[RfReplaceALL]);
     LSrc := ALStringReplaceA(LSrc,'// typedef ULONGLONG HTTP_URL_CONTEXT;','type'#13#10'  HTTP_URL_CONTEXT = ULONGLONG;',[RfReplaceALL]);

     LLst.Text := LSrc;
      For Var i := LLst.Count - 1 downto 0 do
        if ALPosA('// #if ', ALTrim(LLst[I])) = 1 then LLst.Delete(i);
      LSrc := ALTrim(LLst.Text);

     LLst.Text := LSrc;
      For Var i := LLst.Count - 1 downto 0 do
        if ALPosA('// #ifdef ', ALTrim(LLst[I])) = 1 then LLst.Delete(i);
      LSrc := ALTrim(LLst.Text);

     LLst.Text := LSrc;
      For Var i := LLst.Count - 1 downto 0 do
        if ALPosA('// #else ', ALTrim(LLst[I])) = 1 then LLst.Delete(i);
      LSrc := ALTrim(LLst.Text);

     LLst.Text := LSrc;
      For Var i := LLst.Count - 1 downto 0 do
        if ALPosA('// #endif', ALTrim(LLst[I])) = 1 then LLst.Delete(i);
      LSrc := ALTrim(LLst.Text);

     LLst.Text := LSrc;
     var I := 0;
     var LFirst: Boolean := True;
     while i <= LLst.Count - 1 do begin
      if LLst[I] = 'type' then begin
        if not LFirst then begin
          LLst.Delete(i);
          continue;
        end
        else LFirst := False;
        inc(i);
        continue;
      end;
      if ALposA(' ', LLst[I] + ' ') <> 1 then LFirst := True;
      inc(i);
     end;
      LSrc := ALTrim(LLst.Text);


     LSrc := ALStringReplaceA(LSrc,'array[0..HttpHeaderRequestMaximum-1]','array[0..Ord(HTTP_HEADER_ID.HttpHeaderRequestMaximum)-1]',[RfReplaceALL]);
     LSrc := ALStringReplaceA(LSrc,'array[0..HttpHeaderResponseMaximum-1]','array[0..Ord(HTTP_HEADER_ID.HttpHeaderResponseMaximum)-1]',[RfReplaceALL]);
     LSrc := ALStringReplaceA(LSrc,'array[0..HttpRequestSizingTypeMax-1]','array[0..Ord(HTTP_REQUEST_SIZING_TYPE.HttpRequestSizingTypeMax)-1]',[RfReplaceALL]);
     LSrc := ALStringReplaceA(LSrc,'array[0..HttpRequestTimingTypeMax-1]','array[0..Ord(HTTP_REQUEST_TIMING_TYPE.HttpRequestTimingTypeMax)-1]',[RfReplaceALL]);

     LSrc := ALStringReplaceA(LSrc,'// HTTPAPI_LINKAGE'#13#10,#13#10,[RfReplaceALL]);
     LSrc := ALStringReplaceA(LSrc,'// ULONG'#13#10,#13#10,[RfReplaceALL]);
     LSrc := ALStringReplaceA(LSrc,'// WINAPI'#13#10,#13#10,[RfReplaceALL]);
     LSrc := ALStringReplaceA(LSrc,'// BOOL'#13#10,#13#10,[RfReplaceALL]);

     LSrc := ALStringReplaceA(LSrc,#13#10'const ',#13#10'const'#13#10'  ',[RfReplaceALL]);

     LLst.Text := LSrc;
     I := 0;
     LFirst := True;
     while i <= LLst.Count - 1 do begin
      if LLst[I] = 'const' then begin
        if not LFirst then begin
          LLst.Delete(i);
          continue;
        end
        else LFirst := False;
        inc(i);
        continue;
      end;
      if ALposA(' ', LLst[I] + ' ') <> 1 then LFirst := True;
      inc(i);
     end;
      LSrc := ALTrim(LLst.Text);







     P1 := ALPosA('// Define our API linkage.', Lsrc);
     var LStr := ALCopyStr(LSrc, P1, MaxInt);
     delete(LSrc, P1, Maxint);
      LStr := ALStringReplaceA(LStr,#13#10'// Http',#13#10'procedure Http',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,#13#10'//     );',#13#10'     );',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' OPTIONAL,'#13#10,' // OPTIONAL,'#13#10,[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' OPTIONAL'#13#10,' // OPTIONAL'#13#10,[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' OPTIONAL, // ',' // OPTIONAL - ',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' OPTIONAL  // ',' // OPTIONAL - ',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' _Reserved_ IN OUT PVOID ',' _Reserved_ PVOID ',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' _Reserved_ IN ',' _Reserved_ ',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' OUT P',' IN P',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' _Out_ P',' _In_ P',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' _Out_opt_ P',' _In_opt_ P',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' _Outptr_ PWSTR *',' OUT PWSTR ',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' __in ',' IN ',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' __out PVOID __bcount(BufferSize) ',' IN PVOID ',[RfReplaceALL]);

      LStr := ALStringReplaceA(LStr,' _In_reads_bytes_(PropertyInformationLength) ',' _In_ ',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' _In_reads_bytes_(ConfigInformationLength) ',' _In_ ',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' _In_reads_bytes_(ConfigInfoLength) ',' _In_ ',[RfReplaceALL]);
      While ALPosA('_opt_', LStr) > 0 do begin
        var P4 := ALPosA('_opt_', LStr);
        var P5 := ALPosA(#13#10, LStr, P4);
        Insert(' // OPTIONAL', LStr, P5);
        delete(Lstr,P4,4);
      end;
      LStr := ALStringReplaceA(LStr,' _In_reads_bytes_(InputPropertySize) ',' _In_ ',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' _In_reads_bytes_(QualifierSize) ',' _In_ ',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' _In_reads_bytes_(InputLength) ',' _In_ ',[RfReplaceALL]);

      LStr := ALStringReplaceA(LStr,' _In_reads_(EntityChunkCount) P',' _In_ P',[RfReplaceALL]);

      LStr := ALStringReplaceA(LStr,' _Out_writes_bytes_to_(PropertyInformationLength, *ReturnLength) P',' _In_ P',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' _Out_writes_bytes_to_(OutputBufferSize, *BytesReturned) P',' _In_ P',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' _Out_writes_bytes_to_(SslClientCertInfoSize, *BytesReceived) P',' _In_ P',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' _Out_writes_bytes_to_(RequestBufferLength, *BytesReturned) P',' _In_ P',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' _Out_writes_bytes_to_(EntityBufferLength, *BytesReturned) P',' _In_ P',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' _Out_writes_bytes_to_(BufferLength, *BytesRead) P',' _In_ P',[RfReplaceALL]);
      LStr := ALStringReplaceA(LStr,' _Out_writes_bytes_to_(OutputLength, *pReturnLength) P',' _In_ P',[RfReplaceALL]);

      LStr := ALStringReplaceA(LStr,' const VOID *',' PVOID ',[RfReplaceALL]);

     LLst.Text := LStr;
     For I := 0 to LLst.Count - 1 do begin
       Lstr := ALTrim(LLst[i]);
       var P2 := AlPosA(' //', Lstr);
       var LStr2: AnsiString := '';
       if P2 > 0 then begin
        LStr2 := ALCopyStr(Lstr, P2, MaxInt);
        delete(LStr,P2, Maxint);
       end;
       if ALPosA('//     IN ',Lstr) = 1 then begin
         Delete(Lstr, 1, length('//     IN '));
         LLst3.Text := Lstr;
         if LLst3.Count <> 2 then
          Raise Exception.Create('error');
         LLst[i] := '            ' + ALStringReplaceA(LLst3[1],',','',[]) + ': ' + LLst3[0] + ';' + LStr2;
       end;
       if ALPosA('//     _In_ ',Lstr) = 1 then begin
         Delete(Lstr, 1, length('//     _In_ '));
         LLst3.Text := Lstr;
         if LLst3.Count <> 2 then
          Raise Exception.Create('error');
         LLst[i] := '            ' + ALStringReplaceA(LLst3[1],',','',[]) + ': ' + LLst3[0] + ';' + LStr2;
       end;
       if ALPosA('//     _Reserved_ ',Lstr) = 1 then begin
         Delete(Lstr, 1, length('//     _Reserved_ '));
         if LStr2 = '' then LStr2 := ' // Reserved'
         else LStr2 := LStr2 + ' - Reserved';
         LLst3.Text := Lstr;
         if LLst3.Count <> 2 then
          Raise Exception.Create('error');
         LLst[i] := '            ' + ALStringReplaceA(LLst3[1],',','',[]) + ': ' + LLst3[0] + ';' + LStr2;
       end;

       if ALPosA('//     OUT ',Lstr) = 1 then begin
         Delete(Lstr, 1, length('//     OUT '));
         LLst3.Text := Lstr;
         if LLst3.Count <> 2 then
          Raise Exception.Create('error');
         LLst[i] := '            ' + 'out ' + ALStringReplaceA(LLst3[1],',','',[]) + ': ' + LLst3[0] + ';' + LStr2;
       end;

     end;
     LStr := LLst.Text;
     LStr := ALStringReplaceA(LStr,';'#13#10'     );',');',[RfReplaceALL]);
     LStr := ALStringReplaceA(LStr,'; // OPTIONAL'#13#10'     );','); // OPTIONAL',[RfReplaceALL]);
     LStr := ALStringReplaceA(LStr,'; // Reserved'#13#10'     );','); // Reserved',[RfReplaceALL]);

     LSrc := LSrc + LStr;
     LSrc := ALStringReplaceA(LSrc,'// }   // extern "C"'#13#10,#13#10,[RfReplaceALL]);
     LSrc := ALStringReplaceA(LSrc,'// #pragma endregion'#13#10,#13#10,[RfReplaceALL]);





     LLst.Text := LSrc;
      For i := LLst.Count - 1 downto 0 do
        if ALTrim(LLst[I]) = '//' then LLst.Delete(i);
      LSrc := ALTrim(LLst.Text);
     LSrc := ALStringReplaceA(LSrc,#13#10'// //',#13#10'//',[RfReplaceALL]);


     LLst.Text := LSrc;
      I := LLst.Count - 2;
      While i > 1 do begin
        if ALTrim(LLst[I]) = '//' then begin
          if (ALPosA('//', ALTrim(LLst[I+1])) <> 1) and (ALTrim(LLst[I+1]) <> '') then begin
            LLst.Insert(I+1,'');
            inc(i);
          end;
          if (ALPosA('//', ALTrim(LLst[I-1])) <> 1) and (ALTrim(LLst[I-1]) <> '') then begin
            LLst.Insert(I,'');
            inc(i);
          end;
        end;
        dec(i);
      end;
      LSrc := ALTrim(LLst.Text);
     LSrc := ALStringReplaceA(LSrc,#13#10'// //',#13#10'//',[RfReplaceALL]);

     while ALPosA(#13#10#13#10#13#10,LSrc) > 0 do
       LSrc := ALStringReplaceA(LSrc,#13#10#13#10#13#10,#13#10,[RfReplaceALL]);

     LSrc := ALStringReplaceA(LSrc,' Property: ',' &Property: ',[RfReplaceALL]);
     LSrc := ALStringReplaceA(LSrc,'// OPTIONAL,'#13#10,'// OPTIONAL'#13#10,[RfReplaceALL]);
     LSrc := ALStringReplaceA(LSrc,'// NOTE: ', #13#10'// NOTE: ',[RfReplaceALL]);

     ALSaveStringToFile(LSrc, 'c:\Dev\MagicFoundation\Alcinoe\Source\Alcinoe.WinApi.HttpApi.pas');
     LLst.free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
