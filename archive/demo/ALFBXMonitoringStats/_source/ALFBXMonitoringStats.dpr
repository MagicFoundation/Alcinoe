program ALFBXMonitoringStats;

{$APPTYPE CONSOLE}

uses Windows,
     classes,
     SysUtils,
     ALFbxBase,
     ALXmldoc,
     ALString,
     ALStringList,
     ALFBXClient;

{$R *.res}

{**********************************************}
Procedure ALFBXMonitoringStats_StartMainProcess;

  {------------------------------------------------------------------------}
  Function InternalExtractParamFileName(aParamName: AnsiString): AnsiString;
  Var i: Integer;
      ACurrParamStr: AnsiString;
  Begin
    result := '';
    AParamName := AlLowerCase(AParamName) + ':';
    For i := 1 To paramCount do begin
      ACurrParamStr := AnsiString(paramstr(i));
      If AlPos(aParamName,AlLowerCase(ACurrParamStr)) = 1 then begin
        result := AlStringReplace(AlCopyStr(ACurrParamStr,
                                            AlPos(
                                                  ':',
                                                  ACurrParamStr
                                                 ) + 1,
                                            maxint),
                                  '"',
                                  '',
                                  [RfReplaceAll]);
        exit;
      end;
    end;
  end;

  {-------------------------------------------------------------}
  Function InternalIfParamExist(aParamName: AnsiString): boolean;
  Var i: Integer;
  Begin
    result := False;
    AParamName := AlLowerCase(AParamName);
    For i := 1 To paramCount do begin
      result := ALSameText(aParamName,AlLowerCase(AnsiString(paramstr(i))));
      if result then break;
    end;
  end;

  {--------------------------------------------------------------------------}
  function InternalRightPad(aStr: AnsiString; aPadSize : integer): AnsiString;
  var RestLen: Integer;
  begin
    Result := ALTrim(aStr);
    RestLen := aPadSize - Length(Result);
    if RestLen < 1 then Exit;
    Result := Result + StringOfChar(AnsiChar(' '), RestLen);
  end;

  {----------------------------------------------------------}
  function InternalFormatNumber(aStr: AnsiString): AnsiString;
  var aInt: Int64;
  begin
    aInt := ALStrToInt64(aStr);
    if aInt >= 1000000 then result := ALformatfloat('0.0M', aInt / 1000000, AlDefaultFormatSettings)
    else if aInt >= 1000 then result := ALformatfloat('0K', aInt / 1000, AlDefaultFormatSettings)
    else result := astr;
  end;

  {-----------------------------------------------------------------}
  function InternalGetActiveUsers(aXmlData: TalXmlDocument): integer;
  var i: integer;
  begin
    result := 0;
    for I := 0 to aXmlData.DocumentElement.ChildNodes.Count - 1 do
      if aXmlData.DocumentElement.ChildNodes[i].ChildNodes['mon$state'].text = '1' then inc(result);
  end;


var aFbxClient: TalFbxClient;
    aXmlData: TalXmlDocument;
    aXmlRec: TalXmlNode;
    aFormatSettings: TALFormatSettings;
    aFbClientdllPath: AnsiString;
    AlstSQL: TALStringList;
    i, j: integer;
    S1: AnsiString;
Begin

  aFormatSettings := ALDefaultFormatSettings;

  if (InternalExtractParamFileName('-database') = '') or
     (InternalExtractParamFileName('-user') = '') or
     (InternalExtractParamFileName('-password') = '') then begin
    Writeln('usage:   ALFBXMonitoringStats -user:<username> -password:<password> -database:<database> -clientlib:<fbclientdllpath> -ShowStatements');
    exit;
  end;

  aFbClientdllPath := InternalExtractParamFileName('-fbclientdllpath');
  if aFbClientdllPath = '' then aFbClientdllPath := 'fbclient.dll';

  aFbxClient := TalFbxClient.Create(FB25,aFbClientdllPath);
  aXmlData:= TALXmlDocument.create('root');
  Try

    aFbxClient.Connect(InternalExtractParamFileName('-database'),
                       InternalExtractParamFileName('-user'),
                       InternalExtractParamFileName('-password'),
                       'NONE');

    //write the version number
    Writeln(aFbxClient.GetDataBaseInfostring(isc_info_isc_version));
    writeln('');

    //write the number of active transactions
    Writeln('Number of active transactions: ' + ALIntToStr(aFbxClient.GetDataBaseInfoint(isc_info_active_tran_count)));


    //download some statistiques
    //http://www.firebirdsql.org/manual/fbcache-mon-io-stats.html
    aFbxClient.TransactionStart(aFbxClient.DefaultReadTPB);
    Try


      //if we want the SQL statement statistique
      if InternalIfParamExist('-ShowStatements') then begin

        //download from the MON$ Table
        aFbxClient.SelectData('SELECT '+
                                'MON$SQL_TEXT ' +
                              'FROM ' +
                                'MON$STATEMENTS ' +
                              'ORDER BY '+
                                'MON$SQL_TEXT',
                              'rec',
                              aXmlData.DocumentElement,
                              aFormatSettings);

        AlstSQL := TALStringList.create;
        try

          AlstSQL.Sorted := True;
          for i := 0 to aXmlData.DocumentElement.ChildNodes.Count - 1 do begin
            aXmlRec := aXmlData.DocumentElement.ChildNodes[i];
            S1 := axmlrec.childnodes['mon$sql_text'].text;
            J := AlstSQL.IndexOf(S1);
            if J < 0 then AlstSQL.Addobject(S1, pointer(1))
            else AlstSQL.Objects[j] := pointer(integer(AlstSQL.Objects[j]) + 1);
          end;

          Writeln('');
          WRITELN(INTERNALRIGHTPAD('COUNT', 5) + ' ' +
                  INTERNALRIGHTPAD('SQL_TEXT', 220));

          for i := 0 to AlstSQL.Count - 1 do begin
            S1 := AlstSQL[i];
            S1 := AlStringReplace(S1, #13#10, ' ', [rfReplaceALL]);
            S1 := AlStringReplace(S1, #9, ' ', [rfReplaceALL]);
            while AlPos('  ', S1) > 0 do S1 := AlStringReplace(S1, '  ', ' ', [rfReplaceALL]);
            if length(S1) > 220 then S1 := AlCopyStr(S1, 1, 217) + '...';
            Writeln(InternalRightPad(ALIntToStr(integer(AlstSQL.Objects[i])),5) + ' ' +
                    InternalRightPad(S1,220));
          end;

        finally
          aLstSql.Free;
        end;

      end

      //we want the regular statistique
      else begin

        //download from the MON$ Table
        aFbxClient.SelectData('SELECT '+
                                'A1.MON$USER, '+
                                'A1.MON$STATE, '+
                                'A1.MON$REMOTE_ADDRESS, '+
                                'A1.MON$TIMESTAMP, '+
                                'A1.MON$REMOTE_PROCESS, '+
                                'M1.MON$MEMORY_USED, '+             // number of bytes currently in use (High-level memory allocations performed by the engine from its pools. Can be useful for tracing memory leaks and for investigating unusual memory consumption and the attachments, procedures, etc. that might be responsible for it)
                                'M1.MON$MEMORY_ALLOCATED, '+        // number of bytes currently allocated at the OS level (Low-level memory allocations performed by the Firebird memory manager. These are bytes actually allocated by the operating system, so it enables the physical memory consumption to be monitored.)
                                'M1.MON$MAX_MEMORY_USED, '+         // maximum number of bytes used by this object
                                'M1.MON$MAX_MEMORY_ALLOCATED, '+    // maximum number of bytes allocated from the operating system by this object
                                'I1.MON$PAGE_READS, '+              // The number of pages read. These are the pages read from the disc and not from the cache.
                                'I1.MON$PAGE_WRITES, '+             // The number of pages written back to disc.
                                'I1.MON$PAGE_FETCHES, '+            // The number of pages read from the cache as opposed to from disc.
                                'I1.MON$PAGE_MARKS, '+              // The number of pages changed while in the cache. It is possible that not all of these have been written back to disc.
                                'R1.MON$RECORD_SEQ_READS, '+        // number of records read sequentially
                                'R1.MON$RECORD_IDX_READS, '+        // number of records read via an index
                                'R1.MON$RECORD_INSERTS, '+          // number of inserted records
                                'R1.MON$RECORD_UPDATES, '+          // number of updated records
                                'R1.MON$RECORD_DELETES '+           // number of deleted records
                              'FROM ' +
                                'MON$ATTACHMENTS A1 ' +
                              'LEFT JOIN MON$MEMORY_USAGE M1 ON M1.MON$STAT_ID=A1.MON$STAT_ID ' +
                              'LEFT JOIN MON$IO_STATS I1 ON I1.MON$STAT_ID=A1.MON$STAT_ID ' +
                              'LEFT JOIN MON$RECORD_STATS R1 ON R1.MON$STAT_ID=A1.MON$STAT_ID ' +
                              'ORDER BY MON$TIMESTAMP ASC ',
                              'rec',
                              aXmlData.DocumentElement,
                              aFormatSettings);

        Writeln('Number of Users: ' + ALIntToStr(aXmlData.DocumentElement.ChildNodes.Count));
        Writeln('Number of Active Users: ' + ALIntToStr(InternalGetActiveUsers(aXmlData)));
        Writeln('');

        WRITELN(INTERNALRIGHTPAD('USER', 15) + ' ' +
                INTERNALRIGHTPAD('STATE', 5) + ' ' +
                INTERNALRIGHTPAD('REMOTE_ADDRESS', 15) + ' ' +
                INTERNALRIGHTPAD('TIMESTAMP', 22) + ' ' +
                INTERNALRIGHTPAD('REMOTE_PROCESS', 25) + ' ' +
                INTERNALRIGHTPAD('M_USED', 6) + ' ' +
                INTERNALRIGHTPAD('M_ALLOCATED', 11) + ' ' +
                INTERNALRIGHTPAD('M_MAX_USED', 10) + ' ' +
                INTERNALRIGHTPAD('M_MAX_ALLOCATED', 15) + ' ' +
                INTERNALRIGHTPAD('P_READS', 7) + ' ' +
                INTERNALRIGHTPAD('P_WRITES', 8) + ' ' +
                INTERNALRIGHTPAD('P_FETCHES', 9) + ' ' +
                INTERNALRIGHTPAD('P_MARKS', 7) + ' ' +
                INTERNALRIGHTPAD('R_SEQ_READS', 11) + ' ' +
                INTERNALRIGHTPAD('R_IDX_READS', 11) + ' ' +
                INTERNALRIGHTPAD('R_INS', 5) + ' ' +
                INTERNALRIGHTPAD('R_UPD', 5) + ' ' +
                INTERNALRIGHTPAD('R_DEL', 5));

        for i := 0 to aXmlData.DocumentElement.ChildNodes.Count - 1 do begin
          aXmlRec := aXmlData.DocumentElement.ChildNodes[i];
          Writeln(InternalRightPad(axmlrec.childnodes['mon$user'].text, 15) + ' ' +
                  InternalRightPad(axmlrec.childnodes['mon$state'].text,5) + ' ' +
                  InternalRightPad(axmlrec.childnodes['mon$remote_address'].text,15) + ' ' +
                  InternalRightPad(axmlrec.childnodes['mon$timestamp'].text,22) + ' ' +
                  InternalRightPad(ALExtractFileName(axmlrec.childnodes['mon$remote_process'].text), 25) + ' ' +
                  InternalRightPad(ALFormatfloat('0.00', ALStrToInt(axmlrec.childnodes['mon$memory_used'].text) / 1024 / 1024, ALDefaultFormatSettings),6) + ' ' +
                  InternalRightPad(ALFormatfloat('0.00', ALStrToInt(axmlrec.childnodes['mon$memory_allocated'].text) / 1024 / 1024, ALDefaultFormatSettings),11) + ' ' +
                  InternalRightPad(ALFormatfloat('0.00', ALStrToInt(axmlrec.childnodes['mon$max_memory_used'].text) / 1024 / 1024, ALDefaultFormatSettings),10) + ' ' +
                  InternalRightPad(ALFormatfloat('0.00', ALStrToInt(axmlrec.childnodes['mon$max_memory_allocated'].text) / 1024 / 1024, ALDefaultFormatSettings),15) + ' ' +
                  InternalRightPad(InternalFormatNumber(axmlrec.childnodes['mon$page_reads'].text),7) + ' ' +
                  InternalRightPad(InternalFormatNumber(axmlrec.childnodes['mon$page_writes'].text),8) + ' ' +
                  InternalRightPad(InternalFormatNumber(axmlrec.childnodes['mon$page_fetches'].text),9) + ' ' +
                  InternalRightPad(InternalFormatNumber(axmlrec.childnodes['mon$page_marks'].text),7) + ' ' +
                  InternalRightPad(InternalFormatNumber(axmlrec.childnodes['mon$record_seq_reads'].text),11) + ' ' +
                  InternalRightPad(InternalFormatNumber(axmlrec.childnodes['mon$record_idx_reads'].text),11) + ' ' +
                  InternalRightPad(InternalFormatNumber(axmlrec.childnodes['mon$record_inserts'].text),5) + ' ' +
                  InternalRightPad(InternalFormatNumber(axmlrec.childnodes['mon$record_updates'].text),5) + ' ' +
                  InternalRightPad(InternalFormatNumber(axmlrec.childnodes['mon$record_deletes'].text),5));
        end;

        writeln('');
        writeln('');
        writeln('*) M_USED: number of bytes currently in use (High-level memory allocations performed by the engine from its pools)');
        writeln('*) M_ALLOCATED: number of bytes currently allocated at the OS level (Low-level memory allocations performed by the Firebird memory manager)');
        writeln('*) M_MAX_USED: maximum number of bytes used by this object');
        writeln('*) M_MAX_ALLOCATED: maximum number of bytes allocated from the operating system by this object');
        writeln('*) P_READS: The number of pages read. These are the pages read from the disc and not from the cache.');
        writeln('*) P_WRITES: The number of pages written back to disc.');
        writeln('*) P_FETCHES: The number of pages read from the cache as opposed to from disc.');
        writeln('*) P_MARKS: The number of pages changed while in the cache. It is possible that not all of these have been written back to disc.');
        writeln('*) R_SEQ_READS: number of records read sequentially');
        writeln('*) R_IDX_READS: number of records read via an index');
        writeln('*) R_INS: number of inserted records');
        writeln('*) R_UPD: number of updated records');
        writeln('*) R_DEL: number of deleted records');

      end;

      aFbxClient.Transactioncommit;
    Except
      aFbxClient.TransactionRollback;
      raise;
    End;

  Finally
    if aFbxClient.Connected then aFbxClient.Disconnect;
    aFbxClient.free;
    aXmlData.free;
  End;

End;

begin
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

  try
    ALFBXMonitoringStats_StartMainProcess;
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.
