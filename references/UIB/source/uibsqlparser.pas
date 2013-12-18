(********************************************************************************)
(*                        UNIFIED INTERBASE (UIB)                               *)
(*                                                                              *)
(* The contents of this file are subject to the Mozilla Public License Version  *)
(* 1.1 (the "License"); you may not use this file except in compliance with the *)
(* License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ *)
(*                                                                              *)
(* Software distributed under the License is distributed on an "AS IS" basis,   *)
(* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for *)
(* the specific language governing rights and limitations under the License.    *)
(*                                                                              *)
(* Unit owner : Henri Gourvest <hgourvest@progdigy.com>                         *)
(*                                                                              *)
(********************************************************************************)

unit uibsqlparser;
{$I uib.inc}
interface
uses classes, sysutils;

type

  TSQLToken = (toValString, toSymbol, toValNumber, toValFloat, toNotLSS, toNEQ,
    toNotGTR, toLEQ, toLSS, toEQL, toGEQ, toGTR, toConcatenate, toScolon,
    toLParen, toRParen, toComma, toLArray, toRArray, toColon, toPlus, toStar,
    toDot, toVParam, toSlash, toMinus, toACTION, toACTIVE, toADD, toADMIN,
    toAFTER, toALL, toALTER, toAND, toANY, toAS, toASC, toAT, toAUTO, toAUTODDL,
    toAVG, toBACKUP, toBASE_NAME, toBEFORE, toBEGIN, toBETWEEN, toBIGINT,
    toBLOB, toBREAK, toBY, toCACHE, toCASCADE, toCASE, toCAST, toCHAR,
    toCHARACTER, toCHECK, toCHECK_POINT_LENGTH, toCOALESCE, toCOLLATE, toCOLUMN,
    toCOMMIT, toCOMMITTED, toCOMPUTED, toCONDITIONAL, toCONNECT, toCONSTRAINT,
    toCONTAINING, toCOUNT, toCREATE, toCSTRING, toCURRENT, toCURRENT_CONNECTION,
    toCURRENT_DATE, toCURRENT_ROLE, toCURRENT_TIME, toCURRENT_TIMESTAMP,
    toCURRENT_TRANSACTION, toCURRENT_USER, toCURSOR, toDATABASE, toDATE, toDAY,
    toDEBUG, toDEC, toDECIMAL, toDECLARE, toDEFAULT, toDELETE, toDELETING,
    toDESC, toDESCRIPTOR, toDIALECT, toDIFFERENCE, toDISTINCT, toDO, toDOMAIN,
    toDOUBLE, toDROP, toELSE, toEND, toENTRY_POINT, toESCAPE, toEXCEPTION,
    toEXECUTE, toEXISTS, toEXIT, toEXTERNAL, toEXTRACT, toFILE, toFILTER,
    toFIRST, toFLOAT, toFOR, toFOREIGN, toFREE_IT, toFROM, toFULL, toFUNCTION,
    toGDSCODE, toGEN_ID, toGENERATOR, toGRANT, toGROUP, toGROUP_COMMIT_WAIT_TIME,
    toHAVING, toHOUR, toIF, toIN, toINACTIVE, toINDEX, toINNER, toINPUT_TYPE,
    toINSERT, toINSERTING, toINT, toINTEGER, toINTO, toIS, toISOLATION, toJOIN,
    toKEY, toLAST, toLEAVE, toLEFT, toLENGTH, toLEVEL, toLIKE, toLOCK,
    toLOG_BUFFER_SIZE, toLOGFILE, toLONG, toMANUAL, toMAX, toMAXIMUM_SEGMENT,
    toMERGE, toMESSAGE, toMIN, toMINUTE, toMODULE_NAME, toMONTH, toNAMES,
    toNATIONAL, toNATURAL, toNCHAR, toNO, toNOT, toNULL, toNULLIF, toNULLS,
    toNUM_LOG_BUFFERS, toNUMERIC, toOF, toOFF, toON, toONLY, toOPTION, toOR, toORDER,
    toOUTER, toOUTPUT_TYPE, toOVERFLOW, toPAGE, toPAGE_SIZE, toPAGES,
    toPARAMETER, toPASSWORD, toPLAN, toPOSITION, toPOST_EVENT, toPRECISION,
    toPRIMARY, toPRIVILEGES, toPROCEDURE, toPROTECTED, toRAW_PARTITIONS,
    toRDBDBKEY, toREAD, toREAL, toRECORD_VERSION, toRECREATE, toREFERENCES,
    toRELEASE, toRESERVING, toRESTRICT, toRETAIN, toRETURNING_VALUES,
    toRETURNS, toREVOKE, toRIGHT, toROLE, toROLLBACK, toROW_COUNT, toSAVEPOINT,
    toSECOND, toSEGMENT, toSELECT, toSET, toSHADOW, toSHARED,
    toSINGULAR, toSIZE, toSKIP, toSMALLINT, toSNAPSHOT, toSOME, toSORT, toSQL,
    toSQLCODE, toSTABILITY, toSTARTING, toSTATEMENT, toSTATISTICS,
    toSUB_TYPE, toSUBSTRING, toSUM, toSUSPEND, toTABLE, toTHEN, toTIME,
    toTIMESTAMP, toTO, toTRANSACTION, toTRIGGER, toTYPE, toUNCOMMITTED, toUNION,
    toUNIQUE, toUPDATE, toUPDATING, toUPPER, toUSER, toUSING, toVALUE, toVALUES,
    toVARCHAR, toVARIABLE, toVARYING, toVIEW, toWAIT, toWEEKDAY, toWHEN,
    toWHERE, toWHILE, toWITH, toWORK, toWRITE, toYEAR, toYEARDAY,
    toBIT_LENGTH, toBLOCK, toBOTH, toCHAR_LENGTH, toCHARACTER_LENGTH, toCLOSE,
    toCOLLATION, toCOMMENT, toCROSS, toFETCH, toIIF, toLEADING, toLOWER, toNEXT,
    toOCTET_LENGTH, toOPEN, toRESERV, toRESTART, toRETURNING, toROWS, toSCALAR_ARRAY,
    toSCHEMA, toSEQUENCE, toSTARTS, toTRAILING, toTRIM, toEOF);

  TSQLStatement = (ssUnknow, ssAlterException, ssAlterTable, ssAlterTrigger,
    ssAlterProcedure, ssAlterDatabase, ssAlterDomain, ssAlterIndex, ssAlterSequence,
    ssAlterFunction, ssReadBlob, ssInsertBlob, ssCommit, ssDeclareFilter,
    ssDeclareFunction, ssDelete, ssDropException, ssDropIndex, ssDropProcedure,
    ssDropTable, ssDropTrigger, ssDropView, ssDropFilter, ssDropDomain, ssDropFunction,
    ssDropShadow, ssDropRole, ssDropGenerator, ssDropSequence, ssGrant, ssInsertInto,
    ssExecuteProcedure, ssExecuteBlock, ssRecreateProcedure, ssRecreateTable,
    ssRecreateView, ssRecreateException, ssSetSqlDialect, ssSetTransaction,
    ssSetGenerator, ssSetStatistics, ssSetNames, ssCreateException, ssCreateIndex,
    ssCreateProcedure, ssCreateTable, ssCreateTrigger, ssCreateView, ssCreateGenerator,
    ssCreateSequence, ssCreateDatabase, ssCreateDomain, ssCreateShadow, ssCreateRole,
    ssReplaceProcedure, ssReplaceTrigger, ssReplaceException, ssRevoke, ssRollback,
    ssSetSavepoint, ssReleaseSavepoint, ssUndoSavepoint, ssSelect, ssUpdate, ssDebug,
    ssAutoDDL, ssConnect, ssCommentDatabase, ssCommentDomain, ssCommentTable,
    ssCommentView, ssCommentProcedure, ssCommentTrigger, ssCommentFunction,
    ssCommentFilter, ssCommentException, ssCommentGenerator, ssCommentSequence,
    ssCommentIndex, ssCommentRole, ssCommentCharacterSet, ssCommentCollation,
    ssCommentColumn, ssCommentParameter, ssBulkParams, ssEOF);

type
  EUIBSQLParseError = class(Exception)
  private
    FLine: Integer;
    FPos: Integer;
  public
    property Line: integer read FLine;
    property Pos: Integer read FPos;
  end;

  TCommentStyle = (csMultiLine, csSingleLine);

  TOnComment = procedure(sender: TObject; const comment: string; style: TCommentStyle) of object;

  TUIBSQLParser = class
  private
    FStrings: TStrings;
    FParams: TStringlist;
    FStr: string;
    FValue: string;
    FLine: Integer;
    FCursor: PChar;
    FMarkerLine: Integer;
    FMarkerPos: Integer;
    FOnComment: TOnComment;
    function NextLine: boolean;
    procedure Error(const msg: string = '');
    function GetStatement: string;
    procedure Reset;
  public
    constructor Create(AStrings: TStrings); overload; virtual;
    constructor Create(const AString: String); overload; virtual;
    destructor Destroy; override;
    function NextToken: TSQLToken;
    function NextStatement: TSQLStatement;
    property Params: TStringList read FParams;
    property Statement: string read GetStatement;
    property Value: string read FValue;
    property OnComment: TOnComment read FOnComment write FOnComment;
  end;

implementation

const
  CM: array[AnsiChar] of char = (
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', '$', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', ' ', ' ', ' ', ' ', '_',
    ' ', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '
  );

function getCM(c: Char): Char;
begin
{$IFDEF UNICODE}
    if c >= #256 then
      Result := ' ' else
      Result := CM[AnsiChar(c)];
{$ELSE}
    result := CM[c];
{$ENDIF}
end;

{ TUIBSQLParser }

constructor TUIBSQLParser.Create(AStrings: TStrings);
begin
  Assert(AStrings <> nil);
  FParams := TStringList.Create;
  FStrings := TStringList.Create;
  FStrings.Assign(AStrings);
  Reset;
end;

constructor TUIBSQLParser.Create(const AString: String);
begin
  Assert(AString <> '');
  FParams := TStringList.Create;
  FStrings := TStringList.Create;
  FStrings.Text := AString;
  Reset;
end;

destructor TUIBSQLParser.Destroy;
begin
  FStrings.Free;
  FParams.Free;
  inherited;
end;

procedure TUIBSQLParser.Reset;
begin
  FLine := -1;
  FCursor := nil;
  FStr := '';
end;

procedure TUIBSQLParser.Error(const msg: string = '');
var E: EUIBSQLParseError;
begin
  E := EUIBSQLParseError.CreateFmt('Parse error at line %d, pos %d'#13'%s',
    [FLine, FCursor - PChar(FStr), msg]);
  E.FLine := FLine;
  E.FPos := FCursor - PChar(FStr);
  raise E;
end;

function TUIBSQLParser.GetStatement: string;
var
  i: integer;
  str: string;
begin
  result := '';
  for i := FMarkerLine to FLine do
  begin
    str := FStrings[i];
    if (FMarkerLine = FLine) then
      Result := copy(str, FMarkerPos +1, FCursor - PChar(FStr) - FMarkerPos ) else
      if (FMarkerLine = i) then
        Result := copy(str, FMarkerPos + 1, Length(Str) - FMarkerPos) else
        if i <> FLine then
          Result := Result + #13#10 + str else
          Result := Result + #13#10 + copy(str, 0, FCursor - PChar(FStr))
  end;
end;

function TUIBSQLParser.NextLine: boolean;
begin
  inc(FLine);
  result := (FLine < FStrings.Count);
  if result then
  begin
    FStr := FStrings[FLine];
    FCursor := PChar(FStr);
  end;
end;

// simple grammar analyser method
function TUIBSQLParser.NextStatement: TSQLStatement;
var
  LastTock: TSQLToken;
  minus: boolean;

  function Next: TSQLToken;
  begin
    result := NextToken;
    LastTock := Result;
  end;

  procedure SkipTo(t: TSQLToken);
  var tok: TSQLToken;
  begin
    repeat
      tok := Next;
      case tok of
        toBEGIN, toCASE: SkipTo(toEND);
        toDECLARE: SkipTo(toScolon);
      end;
    until (tok = t) or (tok = toEOF);
  end;


begin
  result := ssUnknow;

  if FLine >= 0 then
  begin
    if FCursor^ = #0 then
    begin
      FMarkerLine := FLine + 1;
      FMarkerPos := 0;
    end else
    begin
      FMarkerLine := FLine;
      FMarkerPos := FCursor - PChar(FStr);
    end;
  end else
  begin
    FMarkerLine := 0;
    FMarkerPos := 0;
  end;

  FParams.Clear;
  case Next of
    toALTER:
      case Next of
        toEXCEPTION : result := ssAlterException;
        toTABLE     : result := ssAlterTable;
        toTRIGGER   : result := ssAlterTrigger;
        toPROCEDURE : result := ssAlterProcedure;
        toDATABASE  : result := ssAlterDatabase;
        toDOMAIN    : result := ssAlterDomain;
        toINDEX     : result := ssAlterIndex;
        toSEQUENCE  : result := ssAlterSequence;
        toEXTERNAL  : if (Next = toFUNCTION) then
                        Result := ssAlterFunction;
      end;
    toREAD:
      case Next of
        toBLOB: result := ssReadBlob;
      end;
    toINSERT:
      case Next of
        toBLOB: result := ssInsertBlob;
        toINTO: result := ssInsertInto;
      end;
    toCOMMIT: result := ssCommit;
    toDECLARE:
      case Next of
        toFILTER   : result := ssDeclareFilter;
        toEXTERNAL : result := ssDeclareFunction;
      end;
    toDELETE: result := ssDelete;
    toDROP:
      case Next of
        toEXCEPTION : result := ssDropException;
        toINDEX     : result := ssDropIndex;
        toPROCEDURE : result := ssDropProcedure;
        toTABLE     : result := ssDropTable;
        toTRIGGER   : result := ssDropTrigger;
        toVIEW      : result := ssDropView;
        toFILTER    : result := ssDropFilter;
        toDOMAIN    : result := ssDropDomain;
        toEXTERNAL  : result := ssDropFunction;
        toSHADOW    : result := ssDropShadow;
        toROLE      : result := ssDropRole;
        toGENERATOR : result := ssDropGenerator;
        toSEQUENCE  : result := ssDropSequence;
      end;
    toGRANT: result := ssGrant;
    toEXECUTE:
      case Next of
        toPROCEDURE: result := ssExecuteProcedure;
        toBLOCK    : result := ssExecuteBlock;
      end;
    toRECREATE:
      case Next of
        toPROCEDURE : result := ssRecreateProcedure;
        toTABLE     : result := ssRecreateTable;
        toVIEW      : result := ssRecreateView;
        toEXCEPTION : result := ssRecreateException;
      end;
    toCREATE:
      case Next of
        toEXCEPTION   : result := ssCreateException;
        toUNIQUE,
        toASC,
        toDESC,
        toINDEX       : result := ssCreateIndex;
		    toPROCEDURE   : result := ssCreateProcedure;
		    toTABLE       : result := ssCreateTable;
		    toTRIGGER     : result := ssCreateTrigger;
		    toVIEW        : result := ssCreateView;
		    toGENERATOR   : result := ssCreateGenerator;
        toSEQUENCE    : result := ssCreateSequence;
		    toDATABASE    :
          begin
            if (Next = toValString) then
            begin
              FParams.Values['DATABASE'] := FValue;
              while true do
                case Next of
                  toPAGE_SIZE:
                    case Next of
                      toEQL:
                        if (Next = toValNumber) then
                          FParams.Values['PAGE_SIZE'] := FValue else
                          Error;
                      toValNumber:
                        FParams.Values['PAGE_SIZE'] := FValue;
                    else
                      Error;
                    end;
                  toLENGTH: case Next of
                              toValNumber:
                                FParams.Values['LENGTH'] := FValue;
                              toEQL:
                                if (Next = toValNumber) then
                                  FParams.Values['LENGTH'] := FValue else
                                  Error;
                            else
                              Error;
                            end;
                  toUSER:
                    if (Next = toValString) then
                      FParams.Values['USER'] := FValue else
                      Error;
                  toPASSWORD:
                    if (Next = toValString) then
                      FParams.Values['PASSWORD'] := FValue else
                      Error;
                  toSET: if (Next = toNAMES) and (Next = toValString) then
                           FParams.Values['NAMES'] := FValue else
                           Error;
                  toDEFAULT:
                    if (Next = toCHARACTER) and (Next = toSET) and (Next = toSymbol) then
                      FParams.Values['CHARACTER'] := FValue else
                      Error;

                  toPAGE, toPAGES: ;
                else
                  Break;
                end;
              result := ssCreateDatabase;
            end else
              Error;
          end;
		    toDOMAIN      : result := ssCreateDomain;
		    toSHADOW      : result := ssCreateShadow;
		    toROLE        : result := ssCreateRole;
        toOR          :
          if (Next = toALTER) then
            case Next of
              toPROCEDURE : result := ssReplaceProcedure;
              toTRIGGER   : result := ssReplaceTrigger;
              toEXCEPTION : result := ssReplaceException;
            end;
      end;
    toSET:
      case Next of
        toSQL         :
          if (Next = toDIALECT) and (Next = toValNumber) then
          begin
            FParams.Values['DIALECT'] := FValue;
            result := ssSetSqlDialect;
          end;
        toTRANSACTION : result := ssSetTransaction;
        toGENERATOR   : result := ssSetGenerator;
        toSTATISTICS  : result := ssSetStatistics;
        toNAMES       : begin
                          if (Next = toSymbol) then
                            FParams.Values['CHARACTER'] := FValue else
                            Error;
                          result := ssSetNames;
                        end;
        toAUTODDL     : begin
                          case Next of
                            toON: FParams.Values['AUTODDL'] := 'ON';
                            toOFF: FParams.Values['AUTODDL'] := 'OFF';
                          else
                            error;
                          end;
                          result := ssAutoDDL;
                        end;
      end;
    toREVOKE: result := ssRevoke;
    toROLLBACK: case Next of
                  toWORK: if Next = toTO then
                            begin
                              case Next of
                                toSAVEPOINT: if (Next = toSymbol) then
                                  FParams.Values['SYMBOL'] := FValue else
                                  Error;
                                toSymbol: FParams.Values['SYMBOL'] := FValue;
                              else
                                error;
                              end;
                              result := ssUndoSavepoint
                            end else
                            result := ssRollback;
                  toTO: result := ssUndoSavepoint;
                else
                  result := ssRollback;
                end;
    toSAVEPOINT: if Next = toSymbol then
                 begin
                   FParams.Values['SYMBOL'] := FValue;
                   result := ssSetSavepoint;
                 end else
                   error;
    toRELEASE: if (Next = toSAVEPOINT) and (Next = toSymbol) then
                 begin
                   FParams.Values['SYMBOL'] := FValue;
                   result := ssReleaseSavepoint;
                 end else
                   error;
    toSELECT: result := ssSelect;
    toUPDATE: result := ssUpdate;
    toCOMMENT: if (Next = toON) then
                 case Next of
                   toDATABASE: Result := ssCommentDatabase;
                   toDOMAIN: Result := ssCommentDomain;
                   toTABLE: Result := ssCommentTable;
                   toVIEW: Result := ssCommentView;
                   toPROCEDURE: Result := ssCommentProcedure;
                   toTRIGGER: Result := ssCommentTrigger;
                   toEXTERNAL: if (Next = toFUNCTION) then Result := ssCommentFunction;
                   toFILTER: Result := ssCommentFilter;
                   toEXCEPTION: Result := ssCommentException;
                   toGENERATOR: Result := ssCommentGenerator;
                   toSEQUENCE: Result := ssCommentSequence;
                   toINDEX: Result := ssCommentIndex;
                   toROLE: Result := ssCommentRole;
                   toCHARACTER: if (Next = toSET) then Result := ssCommentCharacterSet;
                   toCOLLATION: Result := ssCommentCollation;
                   toCOLUMN: Result := ssCommentColumn;
                   toPARAMETER: Result := ssCommentParameter;
                 end;
    toDEBUG: result := ssDebug;
    toCONNECT:
      if (Next = toValString) then
      begin
        FParams.Values['DATABASE'] := FValue;
        if (Next = toUSER) and (Next = toValString) then
          FParams.Values['USER'] := FValue else
          error;
        if (Next = toPASSWORD) and (Next = toValString) then
          FParams.Values['PASSWORD'] := FValue else
          error;
        result := ssConnect;
      end else
        error;
    toLParen:
      begin
        minus := false;
        while true do
          case Next of
            toValString, toValNumber, toValFloat, toNULL:
              begin
                if minus then
                begin
                  FParams.AddObject('-' + FValue, Pointer(ord(LastTock)));
                  minus := false;
                end else
                  FParams.AddObject(FValue, Pointer(ord(LastTock)));
                case Next of
                  toRParen: Break;
                  toComma: Continue;
                else
                  Error;
                end;
              end;
            toMinus: minus := true;
            toPlus : minus := false;
          else
            Error;
          end;
        Result := ssBulkParams;
      end;
    toEOF: result := ssEOF;
  else
    Error('Unknow statement type.');
  end;
  if (LastTock <> toScolon) and (LastTock <> toEOF) then
    SkipTo(toScolon);
end;

// lexical analyser method
function TUIBSQLParser.NextToken: TSQLToken;
label next;
var p: PChar;
  function get: char;
  begin
    inc(FCursor);
    Result := getCM(FCursor^);
  end;
begin
  FValue := '';
  result := toEOF;

  if (FCursor = nil) then
    if not NextLine then
      exit;

next:
  case FCursor[0] of
    // Comment /* */
    '/': if (FCursor[1] = '*') then
         begin
           inc(FCursor, 2);
           p := FCursor;
           while true do
           begin
             case FCursor[0] of
               '*': if (FCursor[1] = '/') then
                    begin
                      if assigned(FOnComment) then
                      begin
                        FValue := FValue + copy(p, 0, FCursor - p);
                        FOnComment(self, FValue, csMultiLine);
                      end;
                      inc(FCursor, 2);
                      Break;
                    end else
                      inc(FCursor);
               #00:
                    begin
                      if assigned(FOnComment) then
                        FValue := FValue + copy(p, 0, FCursor - p) + #13#10;
                      if NextLine then
                        p := FCursor else
                        Error('unterminated comment');
                    end;
             else
               inc(FCursor);
             end;
           end;
           goto next;
         end else
         begin
           inc(FCursor);
           result := toSlash; // toSlash
         end;
    // Comment --
    '-': if (FCursor[1] = '-') then
         begin
           if Assigned(FOnComment) then
             FOnComment(self, PChar(@FCursor[2]), csSingleLine);
           if not NextLine then
             exit;
           goto next;
         end else
         begin
           inc(FCursor);
           result := toMinus; // toMinus
         end;
    // toString
    '''':begin
           p := FCursor;
           inc(FCursor);
           while true do
             case FCursor[0] of
               '''': if (FCursor[1] = '''') then
                       inc(FCursor, 2) else
                       begin
                         inc(FCursor);
                         FValue := FValue + copy(p, 0, FCursor - p);
                         result := toValString;
                         Break;
                       end;
               #00: begin
                        FValue := FValue + copy(p, 0, FCursor - p)+ #13#10;
                        if NextLine then
                          p := FCursor else
                          Error('Unterminated string.');
                    end;
             else
               inc(FCursor);
             end;
           FValue := copy(FValue, 2, Length(FValue)-2);
         end;
    // toSymbol (quoted)
    '"': begin
           p := FCursor;
           inc(FCursor);
           while true do
             case FCursor[0] of
               '"': if (FCursor[1] = '"') then
                       inc(FCursor, 2) else
                       begin
                         inc(FCursor);
                         FValue := FValue + copy(p, 0, FCursor - p);
                         result := toSymbol;
                         Break;
                       end;
               #00: begin
                      FValue := FValue + copy(p, 0, FCursor - p) + #13#10;
                      if NextLine then
                        p := FCursor else
                        Error('Unterminated Symbol, expected: ".');
                    end;
             else
               inc(FCursor);
             end;
         end;
    // toNumber or toFloat
    '0'..'9':
         begin
           result := toValNumber;
           p := FCursor;
           repeat;
             inc(FCursor);
             if (FCursor[0] = '.') then
             if {$IFDEF UNICODE} (FCursor[1] < #256) and {$ENDIF} (AnsiChar(FCursor[1]) in ['0'..'9']) then
             begin
               Result := toValFloat;
               inc(FCursor);
               repeat
                 inc(FCursor);
               until not({$IFDEF UNICODE} (FCursor^ < #256) and {$ENDIF} (AnsiChar(FCursor^) in ['0'..'9']));
             end else
             begin
               inc(FCursor);
               Break;
             end;
           until not({$IFDEF UNICODE} (FCursor^ < #256) and {$ENDIF} (AnsiChar(FCursor^) in ['0'..'9','.']));
           FValue := copy(p, 0, FCursor - p);
         end;
    '!', '^', '~':
           begin
           inc(FCursor);
           case FCursor^ of
             '<': result := toNotLSS;
             '=': result := toNEQ;
             '>': result := toNotGTR;
           else
             Error('Expected "<" or "=" or ">".');
           end;
           inc(FCursor);
         end;
    '<': begin
           inc(FCursor);
           case FCursor^ of
             '=': begin
                    inc(FCursor);
                    result := toLEQ;
                  end;
             '>': begin
                    inc(FCursor);
                    result := toNEQ;
                  end;
           else
             result := toLSS;
           end;
         end;
    '=': begin
           inc(FCursor);
           result := toEQL
         end;
    '>': begin
           inc(FCursor);
           case FCursor^ of
             '=': begin
                    inc(FCursor);
                    result := toGEQ;
                  end;
           else
             result := toGTR;
           end;
         end;
    '|': if (FCursor[1] = '|') then
           begin
             inc(FCursor, 2);
             result := toConcatenate;
           end else
             Error('Expected "|".');
    ';': begin inc(FCursor); result := toScolon end;
    '(': begin inc(FCursor); result := toLParen end;
    ')': begin inc(FCursor); result := toRParen end;
    ',': begin inc(FCursor); result := toComma end;
    '[': begin inc(FCursor); result := toLArray end;
    ']': begin inc(FCursor); result := toRArray end;
    ':': begin inc(FCursor); result := toColon end;
    '+': begin inc(FCursor); result := toPlus end;
    '*': begin inc(FCursor); result := toStar end;
    '.': begin inc(FCursor); result := toDot end;
    '?': begin inc(FCursor); result := toVParam end;

    ' ', #9: begin
               inc(FCursor); // separator
               goto next;
             end;

    'a'..'z', 'A'..'Z', '_':
      begin
        result := toSymbol;
        p := FCursor;
        case GetCM(FCursor[0]) of
          'A': case get of
                 'C': if (get = 'T') and (get = 'I') then
                        case get of
                          'O': if (get = 'N') and (get = ' ') then
                                 result := toACTION;
                          'V': if (get = 'E') and (get = ' ') then
                                 result := toACTIVE;
                        end;
                 'D': case get of
                        'D': if (get = ' ') then
                               result := toADD;
                        'M': if (get = 'I') and (get = 'N') and (get = ' ') then
                               result := toADMIN;
                      end;
                 'F': if (get = 'T') and (get = 'E') and (get = 'R') and (get = ' ') then
                        result := toAFTER;
                 'L': case get of
                        'L': if (get = ' ') then
                               result := toALL;
                        'T': if (get = 'E') and (get = 'R') and (get = ' ') then
                               result := toALTER;
                      end;
                 'N': case get of
                        'D': if (get = ' ') then result := toAND;
                        'Y': if (get = ' ') then result := toANY;
                      end;
                 'S': case get of
                        'C': case get of
                               ' ': result := toASC;
                               'E': if (get = 'N') and (get = 'D') and (get = 'I') and
                                      (get = 'N') and (get = 'G') and (get = ' ') then
                                      result := toASC;
                             end;
                        ' ': result := toAS;
                      end;
                 'T': if (get = ' ') then result := toAT;
                 'U': if (get = 'T') and (get = 'O') then
                        case get of
                          ' ': result := toAUTO;
                          'D': if (get = 'D') and (get = 'L') and (get = ' ') then
                                 result := toAUTODDL;
                        end;
                 'V': if (get = 'G') and (get = ' ') then
                        result := toAVG;
               end;
          'B': case get of
                 'A': case get of
                        'C': if (get = 'K') and (get = 'U') and (get = 'P') and (get = ' ') then
                               result := toBACKUP;
                        'S': if (get = 'E') and (get = '_') and (get = 'N') and
                               (get = 'A') and (get = 'M') and (get = 'E') and (get = ' ') then
                               result := toBASE_NAME;
                      end;
                 'E': case get of
                        'F': if (get = 'O') and (get = 'R') and (get = 'E') and (get = ' ') then
                               result := toBEFORE;
                        'G': if (get = 'I') and (get = 'N') and (get = ' ') then
                               result := toBEGIN;
                        'T': if (get = 'W') and (get = 'E') and (get = 'E') and (get = 'N') and (get = ' ') then
                               result := toBETWEEN;
                      end;
                 'I': case get of
                        'T': if (get = '_') and (get = 'L') and (get = 'E') and (get = 'N') and
                               (get = 'G') and (get = 'T') and (get = 'H') and (get = ' ') then
                               Result := toBIT_LENGTH;
                        'G': if (get = 'I') and (get = 'N') and (get = 'T') and (get = ' ') then
                               result := toBIGINT;
                      end;
                 'L': if (get = 'O') then
                      case get of
                        'B': if (get = ' ') then result := toBLOB;
                        'C': if (get = 'K') and (get = ' ') then Result := toBLOCK;
                      end;
                 'O': if (get = 'T') and (get = 'H') and (get = ' ') then
                        result := toBOTH;
                 'R': if (get = 'E') and (get = 'A') and (get = 'K') and (get = ' ') then
                        result := toBREAK;
                 'Y': if (get = ' ') then
                        result := toBY;
              end;
          'C': case get of
                 'A': case get of
                        'C': if (get = 'H') and (get = 'E') and (get = ' ') then
                               result := toCACHE;
                        'S': case get of
                               'C': if (get = 'A') and (get = 'D') and (get = 'E') and (get = ' ') then
                                      result := toCASCADE;
                               'E': if (get = ' ') then
                                      result := toCASE;
                               'T': if (get = ' ') then
                                      result := toCAST;
                             end;
                      end;
                 'H': case get of
                        'A': if (get = 'R') then
                               case get of
                                 ' ': result := toCHAR;
                                 'A': if (get = 'C') and (get = 'T') and (get = 'E') and
                                        (get = 'R') then
                                        case get of
                                          ' ': result := toCHARACTER;
                                          '_': if (get = 'L') and (get = 'E') and (get = 'N') and
                                                 (get = 'G') and (get = 'T') and (get = 'H') and (get = ' ') then
                                                 Result := toCHARACTER_LENGTH;
                                        end;
                                 '_': if (get = 'L') and (get = 'E') and (get = 'N') and
                                        (get = 'G') and (get = 'T') and (get = 'H') and
                                        (get = ' ') then Result := toCHAR_LENGTH;
                               end;
                        'E': if (get = 'C') and (get = 'K') then
                             case get of
                               ' ': result := toCHECK;
                               '_': if (get = 'P') and (get = 'O') and (get = 'I') and
                                      (get = 'N') and (get = 'T') and (get = '_') and
                                      (get = 'L') and (get = 'E') and (get = 'N') and
                                      (get = 'G') and (get = 'T') and (get = 'H') and (get = ' ') then
                                        result := toCHECK_POINT_LENGTH;
                             end;
                      end;
                 'L': if (get = 'O') and (get = 'S') and (get = 'E') and (get = ' ') then Result := toCLOSE;
                 'O': case get of
                        'A': if (get = 'L') and (get = 'E') and (get = 'S') and
                               (get = 'C') and (get = 'E') and (get = ' ') then
                               result := toCOALESCE;
                        'L': case get of
                               'L': if (get = 'A') and (get = 'T') then
                                    case get of
                                      'E': if (get = ' ') then result := toCOLLATE;
                                      'I': if (get = 'O') and (get = 'N') and (get = ' ') then Result := toCOLLATION;
                                    end;
                               'U': if (get = 'M') and (get = 'N') and (get = ' ') then
                                      result := toCOLUMN;
                             end;
                        'M': case get of
                               'M':   case get of
                                        'I': if (get = 'T') then
                                               case get of
                                                 ' ': result := toCOMMIT;
                                                 'T': if (get = 'E') and (get = 'D') and (get = ' ') then
                                                        result := toCOMMITTED;
                                               end;
                                        'E': if (get = 'N') and (get = 'T') and (get = ' ') then Result := toCOMMENT;
                                      end;
                               'P': if (get = 'U') and (get = 'T') and (get = 'E') and
                                       (get = 'D') and (get = ' ') then
                                       result := toCOMPUTED;
                             end;
                        'N': case get of
                               'D': if (get = 'I') and (get = 'T') and (get = 'I') and
                                       (get = 'O') and (get = 'N') and (get = 'A') and
                                       (get = 'L') and (get = ' ') then
                                       result := toCONDITIONAL;
                               'N': if (get = 'E') and (get = 'C') and (get = 'T') and (get = ' ') then
                                       result := toCONNECT;
                               'S': if (get = 'T') and (get = 'R') and (get = 'A') and
                                       (get = 'I') and (get = 'N') and (get = 'T') and (get = ' ') then
                                       result := toCONSTRAINT;
                               'T': if (get = 'A') and (get = 'I') and (get = 'N') and
                                       (get = 'I') and (get = 'N') and (get = 'G') and (get = ' ') then
                                       result := toCONTAINING;
                             end;
                        'U': if (get = 'N') and (get = 'T') and (get = ' ') then
                               result := toCOUNT;
                      end;
                 'R': case get of
                        'E': if (get = 'A') and (get = 'T') and (get = 'E') and (get = ' ') then  result := toCREATE;
                        'O': if (get = 'S') and (get = 'S') and (get = ' ') then Result := toCROSS;
                      end;
                 'S': if (get = 'T') and (get = 'R') and (get = 'I') and (get = 'N') and (get = 'G') and (get = ' ') then
                        result := toCSTRING;
                 'U': if (get = 'R') then
                        case get of
                          'R': if (get = 'E') and (get = 'N') and (get = 'T') then
                                 case get of
                                   ' ': result := toCURRENT;
                                   '_': case get of
                                          'C': if (get = 'O') and (get = 'N') and (get = 'N') and (get = 'E') and (get = 'C') and
                                                  (get = 'T') and (get = 'I') and (get = 'O') and (get = 'N') and (get = ' ') then
                                                  result := toCURRENT_CONNECTION;
                                          'D': if (get = 'A') and (get = 'T') and (get = 'E') and (get = ' ') then
                                                 result := toCURRENT_DATE;
                                          'R': if (get = 'O') and (get = 'L') and (get = 'E') and (get = ' ') then
                                                 result := toCURRENT_ROLE;
                                          'T': case get of
                                                 'I': if (get = 'M') and (get = 'E') then
                                                      case get of
                                                        ' ': result := toCURRENT_TIME;
                                                        'S': if (get = 'T') and (get = 'A') and
                                                                (get = 'M') and (get = 'P') and (get = ' ') then
                                                               result := toCURRENT_TIMESTAMP;
                                                      end;
                                                 'R': if (get = 'A') and (get = 'N') and (get = 'S') and (get = 'A') and
                                                        (get = 'C') and (get = 'T') and (get = 'I') and (get = 'O') and
                                                        (get = 'N') and (get = ' ') then
                                                        result := toCURRENT_TRANSACTION;
                                               end;
                                          'U': if (get = 'S') and (get = 'E') and (get = 'R') and (get = ' ') then
                                                 result := toCURRENT_USER;
                                        end;
                                 end;
                          'S': if (get = 'O') and (get = 'R') and (get = ' ') then
                                 result := toCURSOR;
                        end;
               end;
          'D': case get of
                 'A': case get of
                        'T': case get of
                               'A': if (get = 'B') and (get = 'A') and (get = 'S') and
                                       (get = 'E') and (get = ' ') then
                                      result := toDATABASE;
                               'E': if (get = ' ') then
                                      result := toDATE;
                             end;
                        'Y': if (get = ' ') then
                               result := toDAY;
                      end;
                 'E': case get of
                        'B': if (get = 'U') and (get = 'G') and (get = ' ') then
                               result := toDEBUG;
                        'C': case get of
                               ' ': result := toDEC;
                               'I': if (get = 'M') and (get = 'A') and (get = 'L') and (get = ' ') then
                                      result := toDECIMAL;
                               'L': if (get = 'A') and (get = 'R') and (get = 'E') and (get = ' ') then
                                      result := toDECLARE;
                             end;
                        'F': if (get = 'A') and (get = 'U') and (get = 'L') and (get = 'T') and (get = ' ') then
                               result := toDEFAULT;
                        'L': if (get = 'E') and (get = 'T') then
                               case get of
                                 'E': if (get = ' ') then result := toDELETE;
                                 'I': if (get = 'N') and (get = 'G') and (get = ' ') then
                                        result := toDELETING;
                               end;
                        'S': if (get = 'C') then
                               case get of
                                 ' ': result := toDESC;
                                 'E': if (get = 'N') and (get = 'D') and (get = 'I') and
                                         (get = 'N') and (get = 'G') and (get = ' ') then
                                        result := toDESC;
                                 'R': if (get = 'I') and (get = 'P') and (get = 'T') and
                                         (get = 'O') and (get = 'R') and (get = ' ') then
                                        result := toDESCRIPTOR;
                               end;
                      end;
                 'I': case get of
                        'A': if (get = 'L') and (get = 'E') and (get = 'C') and
                                (get = 'T') and (get = ' ') then
                               result := toDIALECT;
                        'F': if (get = 'F') and (get = 'E') and (get = 'R') and
                                (get = 'E') and (get = 'N') and (get = 'C') and
                                (get = 'E') and (get = ' ') then
                               result := toDIFFERENCE;
                        'S': if (get = 'T') and (get = 'I') and (get = 'N') and
                                (get = 'C') and (get = 'T') and (get = ' ') then
                               result := toDISTINCT;
                      end;
                 'O': case get of
                        ' ': result := toDO;
                        'M': if (get = 'A') and (get = 'I') and (get = 'N') and (get = ' ') then
                               result := toDOMAIN;
                        'U': if (get = 'B') and (get = 'L') and (get = 'E') and (get = ' ') then
                               result := toDOUBLE;
                      end;
                 'R': if (get = 'O') and (get = 'P') and (get = ' ') then
                        result := toDROP;
               end;
          'E': case get of
                 'L': if (get = 'S') and (get = 'E') and (get = ' ') then
                       result := toELSE;
                 'N': case get of
                        'D': if (get = ' ') then result := toEND;
                        'T': if (get = 'R') and (get = 'Y') and (get = '_') and (get = 'P') and
                                (get = 'O') and (get = 'I') and (get = 'N') and (get = 'T') and (get = ' ') then
                               result := toENTRY_POINT;
                      end;
                 'S': if (get = 'C') and (get = 'A') and (get = 'P') and (get = 'E') and (get = ' ') then
                        result := toESCAPE;
                 'X': case get of
                        'C': if (get = 'E') and (get = 'P') and (get = 'T') and (get = 'I') and
                                (get = 'O') and (get = 'N') and (get = ' ') then
                               result := toEXCEPTION;
                        'E': if (get = 'C') and (get = 'U') and (get = 'T') and (get = 'E') and (get = ' ') then
                               result := toEXECUTE;
                        'I': case get of
                               'S': if (get = 'T') and (get = 'S') and (get = ' ') then
                                      result := toEXISTS;
                               'T': if (get = ' ') then
                                      result := toEXIT;
                             end;
                        'T': case get of
                               'E': if (get = 'R') and (get = 'N') and (get = 'A') and (get = 'L') and (get = ' ') then
                                      result := toEXTERNAL;
                               'R': if (get = 'A') and (get = 'C') and (get = 'T') and (get = ' ') then
                                      result := toEXTRACT;
                             end;
                      end;
               end;
          'F': case get of
                 'E': if (get = 'T') and (get = 'C') and (get = 'H') and (get = ' ') then Result := toFETCH;
                 'I': case get of
                        'L': case get of
                               'E': if (get = ' ') then
                                      result := toFILE;
                               'T': if (get = 'E') and (get = 'R') and (get = ' ') then
                                      result := toFILTER;
                             end;
                        'R': if (get = 'S') and (get = 'T') and (get = ' ') then
                               result := toFIRST;
                      end;
                 'L': if (get = 'O') and (get = 'A') and (get = 'T') and (get = ' ') then
                        result := toFLOAT;
                 'O': if (get = 'R') then
                        case get of
                          ' ': result := toFOR;
                          'E': if (get = 'I') and (get = 'G') and (get = 'N') and (get = ' ') then
                                 result := toFOREIGN;
                        end;
                 'R': case get of
                        'E': if (get = 'E') and (get = '_') and (get = 'I') and (get = 'T') and (get = ' ') then
                               result := toFREE_IT;
                        'O': if (get = 'M') and (get = ' ') then
                               result := toFROM;
                      end;
                 'U': case get of
                        'L': if (get = 'L') and (get = ' ') then
                               result := toFULL;
                        'N': if (get = 'C') and (get = 'T') and (get = 'I') and
                                (get = 'O') and (get = 'N') and (get = ' ') then
                               result := toFUNCTION;
                      end;
               end;
          'G': case get of
                 'D': if (get = 'S') and (get = 'C') and (get = 'O') and (get = 'D') and (get = 'E') and (get = ' ') then
                        result := toGDSCODE;
                 'E': if (get = 'N') then
                        case get of
                          '_': if (get = 'I') and (get = 'D') and (get = ' ') then
                                 result := toGEN_ID;
                          'E': if (get = 'R') and (get = 'A') and (get = 'T') and
                                  (get = 'O') and (get = 'R') and (get = ' ') then
                                 result := toGENERATOR;
                        end;
                 'R': case get of
                        'A': if (get = 'N') and (get = 'T') and (get = ' ') then
                               result := toGRANT;
                        'O': if (get = 'U') and (get = 'P') then
                               case get of
                                 ' ': result := toGROUP;
                                 '_': if (get = 'C') and (get = 'O') and (get = 'M') and (get = 'M') and
                                         (get = 'I') and (get = 'T') and (get = '_') and (get = 'W') and
                                         (get = 'A') and (get = 'I') and (get = 'T') and (get = '_') and
                                         (get = 'T') and (get = 'I') and (get = 'M') and (get = 'E') and (get = ' ') then
                                        result := toGROUP_COMMIT_WAIT_TIME;
                               end;
                      end;
               end;
          'H': case get of
                 'A': if (get = 'V') and (get = 'I') and (get = 'N') and (get = 'G') and (get = ' ') then
                        result := toHAVING;
                 'O': if (get = 'U') and (get = 'R') and (get = ' ') then
                        result := toHOUR;
               end;
          'I': case get of
                 'F': if (get = ' ') then result := toIF;
                 'I': if (get = 'F') and (get = ' ') then result := toIIF;
                 'N': case get of
                        ' ': result := toIN;
                        'A': if (get = 'C') and (get = 'T') and (get = 'I') and
                                (get = 'V') and (get = 'E') and (get = ' ') then
                               result := toINACTIVE;
                        'D': if (get = 'E') and (get = 'X') and (get = ' ') then
                               result := toINDEX;
                        'N': if (get = 'E') and (get = 'R') and (get = ' ') then
                               result := toINNER;
                        'P': if (get = 'U') and (get = 'T') and (get = '_') and
                                (get = 'T') and (get = 'Y') and (get = 'P') and
                                (get = 'E') and (get = ' ') then
                               result := toINPUT_TYPE;
                        'S': if (get = 'E') and (get = 'R') and (get = 'T') then
                               case get of
                                 ' ': result := toINSERT;
                                 'I': if (get = 'N') and (get = 'G') and (get = ' ') then
                                        result := toINSERTING;
                               end;
                        'T': case get of
                               ' ': result := toINT;
                               'E': if (get = 'G') and (get = 'E') and (get = 'R') and (get = ' ') then
                                      result := toINTEGER;
                               'O': if (get = ' ') then
                                      result := toINTO;
                             end;
                        end;
                 'S': case get of
                        ' ': result := toIS;
                        'O': if (get = 'L') and (get = 'A') and (get = 'T') and
                                (get = 'I') and (get = 'O') and (get = 'N') and (get = ' ') then
                               result := toISOLATION;
                      end;
               end;
          'J': if (get = 'O') and (get = 'I') and (get = 'N') and (get = ' ') then
                 result := toJOIN;
          'K': if (get = 'E') and (get = 'Y') and (get = ' ') then
                 result := toKEY;
          'L': case get of
                 'A': if (get = 'S') and (get = 'T') and (get = ' ') then
                        result := toLAST;
                 'E': case get of
                        'A': case get of
                               'V': if (get = 'E') and (get = ' ') then result := toLEAVE;
                               'D': if (get = 'I') and (get = 'N') and (get = 'G') and (get = ' ') then Result := toLEADING;
                             end;
                        'F': if (get = 'T') and (get = ' ') then result := toLEFT;
                        'N': if (get = 'G') and (get = 'T') and (get = 'H') and (get = ' ') then result := toLENGTH;
                        'V': if (get = 'E') and (get = 'L') and (get = ' ') then result := toLEVEL;
                      end;
                 'I': if (get = 'K') and (get = 'E') and (get = ' ') then
                        result := toLIKE;
                 'O': case get of
                        'C': if (get = 'K') and (get = ' ') then
                               result := toLOCK;
                        'G': case get of
                               '_': if (get = 'B') and (get = 'U') and (get = 'F') and (get = 'F') and
                                       (get = 'E') and (get = 'R') and (get = '_') and (get = 'S') and
                                       (get = 'I') and (get = 'Z') and (get = 'E') and (get = ' ') then
                                      result := toLOG_BUFFER_SIZE;
                               'F': if (get = 'I') and (get = 'L') and (get = 'E') and (get = ' ') then
                                      result := toLOGFILE;
                             end;
                        'N': if (get = 'G') and (get = ' ') then
                               result := toLONG;
                        'W': if (get = 'E') and (get = 'R') and (get = ' ') then Result := toLOWER;
                      end;
               end;
          'M': case get of
                 'A': case get of
                        'N': if (get = 'U') and (get = 'A') and (get = 'L') and (get = ' ') then
                               result := toMANUAL;
                        'X': case get of
                               ' ': result := toMAX;
                               'I': if (get = 'M') and (get = 'U') and (get = 'M') and
                                    (get = '_') and (get = 'S') and (get = 'E') and
                                    (get = 'G') and (get = 'M') and (get = 'E') and
                                    (get = 'N') and (get = 'T') and (get = ' ') then
                                   result := toMAXIMUM_SEGMENT;
                             end;
                      end;
                 'E': case get of
                        'R': if (get = 'G') and (get = 'E') and (get = ' ') then
                               result := toMERGE;
                        'S': if (get = 'S') and (get = 'A') and (get = 'G') and
                                (get = 'E') and (get = ' ') then
                               result := toMESSAGE;
                      end;
                 'I': if (get = 'N') then
                        case get of
                          ' ': result := toMIN;
                          'U': if (get = 'T') and (get = 'E') and (get = ' ') then
                                 result := toMINUTE;
                        end;
                 'O': case get of
                        'D': if (get = 'U') and (get = 'L') and (get = 'E') and
                                (get = '_') and (get = 'N') and (get = 'A') and
                                (get = 'M') and (get = 'E') and (get = ' ') then
                               result := toMODULE_NAME;
                        'N': if (get = 'T') and (get = 'H') and (get = ' ') then
                               result := toMONTH;
                      end;
               end;
          'N': case get of
                 'A': case get of
                        'M': if (get = 'E') and (get = 'S') and (get = ' ') then
                               result := toNAMES;
                        'T': case get of
                               'I': if (get = 'O') and (get = 'N') and (get = 'A') and
                                       (get = 'L') and (get = ' ') then
                                      result := toNATIONAL;
                               'U': if (get = 'R') and (get = 'A') and (get = 'L') and (get = ' ') then
                                      result := toNATURAL;
                             end;
                      end;
                 'C': if (get = 'H') and (get = 'A') and (get = 'R') and (get = ' ') then
                        result := toNCHAR;
                 'E': if (get = 'X') and (get = 'T') and (get = ' ') then Result := toNEXT;
                 'O': case get of
                        ' ': result := toNO;
                        'T': if (get = ' ') then result := toNOT;
                      end;
                 'U': case get of
                        'L': if (get = 'L') then
                               case get of
                                 ' ': result := toNULL;
                                 'I': if (get = 'F') and (get = ' ') then result := toNULLIF;
                                 'S': if (get = ' ') then result := toNULLS;
                               end;
                        'M': case get of
                               '_': if (get = 'L') and (get = 'O') and (get = 'G') and (get = '_') and
                                       (get = 'B') and (get = 'U') and (get = 'F') and (get = 'F') and
                                       (get = 'E') and (get = 'R') and (get = 'S') and (get = ' ') then
                                      result := toNUM_LOG_BUFFERS;
                               'E': if (get = 'R') and (get = 'I') and (get = 'C') and (get = ' ') then
                                      result := toNUMERIC;
                             end;
                      end;
               end;
          'O': case get of
                 'C': if (get = 'T') and (get = 'E') and (get = 'T') and (get = '_') and (get = 'L') and
                        (get = 'E') and (get = 'N') and (get = 'G') and (get = 'T') and (get = 'H') and
                        (get = ' ') then Result := toOCTET_LENGTH;
                 'F': case get of
                        ' ': result := toOF;
                        'F': if get = ' ' then result := toOFF;
                      end;
                 'N': case get of
                        ' ': result := toON;
                        'L': if (get = 'Y') and (get = ' ') then result := toONLY;
                      end;
                 'P': case get of
                        'E': if (get = 'N') and (get = ' ') then Result := toOPEN;
                        'T': if (get = 'I') and (get = 'O') and (get = 'N') and (get = ' ') then result := toOPTION;
                      end;
                 'R': case get of
                        ' ': result := toOR;
                        'D': if (get = 'E') and (get = 'R') and (get = ' ') then
                               result := toORDER;
                      end;
                 'U': if (get = 'T') then
                        case get of
                          'E': if (get = 'R') and (get = ' ') then
                                 result := toOUTER;
                          'P': if (get = 'U') and (get = 'T') and (get = '_') and (get = 'T') and
                                 (get = 'Y') and (get = 'P') and (get = 'E') and (get = ' ') then
                                 result := toOUTPUT_TYPE;
                        end;
                 'V': if (get = 'E') and (get = 'R') and (get = 'F') and (get = 'L') and
                         (get = 'O') and (get = 'W') and (get = ' ') then
                        result := toOVERFLOW;
               end;
          'P': case get of
                 'A': case get of
                        'G': if (get = 'E') then
                               case get of
                                 ' ': result := toPAGE;
                                 '_': if (get = 'S') and (get = 'I') and (get = 'Z') and
                                         (get = 'E') and (get = ' ') then
                                        result := toPAGE_SIZE;
                                 'S': if (get = ' ') then result := toPAGES;
                               end;
                        'R': if (get = 'A') and (get = 'M') and (get = 'E') and (get = 'T') and
                                (get = 'E') and (get = 'R') and (get = ' ') then
                               result := toPARAMETER;
                        'S': if (get = 'S') and (get = 'W') and (get = 'O') and
                                (get = 'R') and (get = 'D') and (get = ' ') then
                               result := toPASSWORD;
                      end;
                 'L': if (get = 'A') and (get = 'N') and (get = ' ') then
                        result := toPLAN;
                 'O': if (get = 'S') then
                        case get of
                          'I': if (get = 'T') and (get = 'I') and (get = 'O') and
                                  (get = 'N') and (get = ' ') then
                                 result := toPOSITION;
                          'T': if (get = '_') and (get = 'E') and (get = 'V') and
                                  (get = 'E') and (get = 'N') and (get = 'T') and (get = ' ') then
                                 result := toPOST_EVENT;
                        end;
                 'R': case get of
                        'E': if (get = 'C') and (get = 'I') and (get = 'S') and (get = 'I') and
                                (get = 'O') and (get = 'N') and (get = ' ') then
                               result := toPRECISION;
                        'I': case get of
                               'M': if (get = 'A') and (get = 'R') and (get = 'Y') and (get = ' ') then
                                      result := toPRIMARY;
                               'V': if (get = 'I') and (get = 'L') and (get = 'E') and (get = 'G') and
                                       (get = 'E') and (get = 'S') and (get = ' ') then
                                      result := toPRIVILEGES;
                             end;
                        'O': case get of
                               'C': if (get = 'E') and (get = 'D') and (get = 'U') and
                                       (get = 'R') and (get = 'E') and (get = ' ') then
                                      result := toPROCEDURE;
                               'T': if (get = 'E') and (get = 'C') and (get = 'T') and
                                       (get = 'E') and (get = 'D') and (get = ' ') then
                                      result := toPROTECTED;
                             end;
                      end;
               end;
          'R': case get of
                 'A': if (get = 'W') and (get = '_') and (get = 'P') and (get = 'A') and
                         (get = 'R') and (get = 'T') and (get = 'I') and (get = 'T') and
                         (get = 'I') and (get = 'O') and (get = 'N') and (get = 'S') and
                         (get = ' ') then
                        result := toRAW_PARTITIONS;
                 'D': if (get = 'B') and (get = '$') and (get = 'D') and (get = 'B') and
                         (get = '_') and (get = 'K') and (get = 'E') and (get = 'Y') and
                         (get = ' ') then
                        result := toRDBDBKEY;
                 'E': case get of
                        'A': case get of
                               'D': if (get = ' ') then result := toREAD;
                               'L': if (get = ' ') then result := toREAL;
                             end;
                        'C': case get of
                               'O': if (get = 'R') and (get = 'D') and (get = '_') and (get = 'V') and
                                       (get = 'E') and (get = 'R') and (get = 'S') and (get = 'I') and
                                       (get = 'O') and (get = 'N') and (get = ' ') then
                                      result := toRECORD_VERSION;
                               'R': if (get = 'E') and (get = 'A') and (get = 'T') and (get = 'E') and (get = ' ') then
                                      result := toRECREATE;
                             end;
                        'F': if (get = 'E') and (get = 'R') and (get = 'E') and (get = 'N') and
                                (get = 'C') and (get = 'E') and (get = 'S') and (get = ' ') then
                               result := toREFERENCES;
                        'L': if (get = 'E') and (get = 'A') and (get = 'S') and (get = 'E') and (get = ' ') then
                               result := toRELEASE;
                        'S': case get of
                               'E': if (get = 'R') and (get = 'V') then
                                      case get of
                                        ' ': result := toRESERVING;
                                        'I': if (get = 'N') and (get = 'G') and (get = ' ') then
                                               result := toRESERVING;
                                      end;
                               'T': case get of
                                      'A': if (get = 'R') and (get = 'T') and (get = ' ') then Result := toRESTART;
                                      'R': if (get = 'I') and (get = 'C') and (get = 'T') and (get = ' ') then result := toRESTRICT;
                                    end;
                             end;
                        'T': case get of
                               'A': if (get = 'I') and (get = 'N') and (get = ' ') then
                                      result := toRETAIN;
                               'U': if (get = 'R') and (get = 'N') then
                                      case get of
                                        'I': if (get = 'N') and (get = 'G') then
                                                case get of
                                                  ' ': Result := toRETURNING;
                                                  '_': if (get = 'V') and (get = 'A') and (get = 'L') and (get = 'U') and (get = 'E') and
                                                         (get = 'S') and (get = ' ') then result := toRETURNING_VALUES;
                                                end;
                                        'S': if (get = ' ') then result := toRETURNS;

                                      end;
                             end;
                        'V': if (get = 'O') and (get = 'K') and (get = 'E') and (get = ' ') then
                               result := toREVOKE;
                      end;
                 'I': if (get = 'G') and (get = 'H') and (get = 'T') and (get = ' ') then
                        result := toRIGHT;
                 'O': case get of
                        'L': case get of
                               'E': if (get = ' ') then result := toROLE;
                               'L': if (get = 'B') and (get = 'A') and (get = 'C') and (get = 'K') and (get = ' ') then
                                      result := toROLLBACK;
                             end;
                        'W': case get of
                               '_': if (get = 'C') and (get = 'O') and (get = 'U') and
                                     (get = 'N') and (get = 'T') and (get = ' ') then result := toROW_COUNT;
                               'S': if (get = ' ') then Result := toROWS;
                             end;
                      end;
               end;
          'S': case get of
                 'A': if (get = 'V') and (get = 'E') and (get = 'P') and (get = 'O') and
                         (get = 'I') and (get = 'N') and (get = 'T') and (get = ' ') then
                        result := toSAVEPOINT;
                 'C': case get of
                        'A': if (get = 'L') and (get = 'A') and (get = 'R') and (get = '_') and (get = 'A') and
                               (get = 'R') and (get = 'R') and (get = 'A') and (get = 'Y') and (get = ' ') then Result := toSCALAR_ARRAY;
                        'H': if (get = 'E') and (get = 'M') and (get = 'A') and (get = ' ') then result := toDATABASE;
                      end;
                 'E': case get of
                        'C': if (get = 'O') and (get = 'N') and (get = 'D') and (get = ' ') then
                               result := toSECOND;
                        'G': if (get = 'M') and (get = 'E') and (get = 'N') and (get = 'T') and (get = ' ') then
                               result := toSEGMENT;
                        'L': if (get = 'E') and (get = 'C') and (get = 'T') and (get = ' ') then
                               result := toSELECT;
                        'Q': if (get = 'U') and (get = 'E') and (get = 'N') and (get = 'C') and (get = 'E') and (get = ' ') then
                               Result := toSEQUENCE;
                        'T': if (get = ' ') then result := toSET;
                      end;
                 'H': if (get = 'A') then
                        case get of
                          'D': if (get = 'O') and (get = 'W') and (get = ' ') then
                                 result := toSHADOW;
                          'R': if (get = 'E') and (get = 'D') and (get = ' ') then
                                 result := toSHARED;
                        end;
                 'I': case get of
                        'N': if (get = 'G') and (get = 'U') and (get = 'L') and (get = 'A') and (get = 'R') and (get = ' ') then
                               result := toSINGULAR;
                        'Z': if (get = 'E') and (get = ' ') then
                               result := toSIZE;
                      end;
                 'K': if (get = 'I') and (get = 'P') and (get = ' ') then
                        result := toSKIP;
                 'M': if (get = 'A') and (get = 'L') and (get = 'L') and (get = 'I') and
                         (get = 'N') and (get = 'T') and (get = ' ') then
                        result := toSMALLINT;
                 'N': if (get = 'A') and (get = 'P') and (get = 'S') and (get = 'H') and
                         (get = 'O') and (get = 'T') and (get = ' ') then
                        result := toSNAPSHOT;
                 'O': case get of
                        'M': if (get = 'E') and (get = ' ') then result := toSOME;
                        'R': if (get = 'T') and (get = ' ') then result := toSORT;
                      end;
                 'Q': if (get = 'L') then
                        case get of
                          ' ': result := toSQL;
                          'C': if (get = 'O') and (get = 'D') and (get = 'E') and (get = ' ') then
                                 result := toSQLCODE;
                        end;
                 'T': case get of
                      'A':
                        case get of
                          'B': if (get = 'I') and (get = 'L') and (get = 'I') and (get = 'T') and
                                  (get = 'Y') and (get = ' ') then
                                 result := toSTABILITY;
                          'R': if (get = 'T') then
                                 case get of
                                   'I': if (get = 'N') and (get = 'G') and (get = ' ') then
                                          result := toSTARTING;
                                   'S': if (get = ' ') then
                                           result := toSTARTING;
                                 end;
                          'T': case get of
                                 'E': if (get = 'M') and (get = 'E') and (get = 'N') and (get = 'T') and (get = ' ') then
                                        result := toSTATEMENT;
                                 'I': if (get = 'S') and (get = 'T') and (get = 'I') and
                                         (get = 'C') and (get = 'S') and (get = ' ') then
                                        result := toSTATISTICS;
                               end;
                        end;
                      end;
                 'U': case get of
                        'B': case get of
                               '_': if (get = 'T') and (get = 'Y') and (get = 'P') and
                                       (get = 'E') and (get = ' ') then
                                      result := toSUB_TYPE;
                               'S': if (get = 'T') and (get = 'R') and (get = 'I') and
                                       (get = 'N') and (get = 'G') and (get = ' ') then
                                      result := toSUBSTRING;
                             end;
                        'M': if (get = ' ') then Result := toSUM;
                        'S': if (get = 'P') and (get = 'E') and (get = 'N') and
                                (get = 'D') and (get = ' ') then
                               result := toSUSPEND;
                      end;
               end;
          'T': case get of
                 'A': if (get = 'B') and (get = 'L') and (get = 'E') and (get = ' ') then
                        result := toTABLE;
                 'H': if (get = 'E') and (get = 'N') and (get = ' ') then
                        result := toTHEN;
                 'I': if (get = 'M') and (get = 'E') then
                        case get of
                          ' ': result := toTIME;
                          'S': if (get = 'T') and (get = 'A') and (get = 'M') and (get = 'P') and (get = ' ') then
                                 result := toTIMESTAMP;
                        end;
                 'O': if (get = ' ') then
                        result := toTO;
                 'R': case get of
                        'A': case get of
                               'I': if (get = 'L') and (get = 'I') and (get = 'N') and (get = 'G') and (get = ' ') then
                                      Result := toTRAILING;
                               'N': if (get = 'S') and (get = 'A') and (get = 'C') and
                                      (get = 'T') and (get = 'I') and (get = 'O') and (get = 'N') and
                                      (get = ' ') then result := toTRANSACTION;
                             end;
                        'I': case get of
                               'G': if (get = 'G') and (get = 'E') and (get = 'R') and (get = ' ') then result := toTRIGGER;
                               'M': if (get = ' ') then Result := toTRIM;
                             end;
                      end;
                 'Y': if (get = 'P') and (get = 'E') and (get = ' ') then
                        result := toTYPE;
               end;
          'U': case get of
                 'N': case get of
                        'C': if (get = 'O') and (get = 'M') and (get = 'M') and
                                (get = 'I') and (get = 'T') and (get = 'T') and
                                (get = 'E') and (get = 'D') and (get = ' ') then
                               result :=toUNCOMMITTED;
                        'I': case get of
                               'O': if (get = 'N') and (get = ' ') then
                                      result := toUNION;
                               'Q': if (get = 'U') and (get = 'E') and (get = ' ') then
                                      result := toUNIQUE;
                             end;
                      end;
                 'P': case get of
                        'D': if (get = 'A') and (get = 'T') then
                               case get of
                                 'E': if (get = ' ') then result := toUPDATE;
                                 'I': if (get = 'N') and (get = 'G') and (get = ' ') then
                                        result := toUPDATING;
                               end;
                        'P': if (get = 'E') and (get = 'R') and (get = ' ') then
                                result := toUPPER;
                      end;
                 'S': case get of
                        'E': if (get = 'R') and (get = ' ') then
                               result := toUSER;
                        'I': if (get = 'N') and (get = 'G') and (get = ' ') then
                               result := toUSING;
                      end;
               end;
          'V': case get of
                 'A': case get of
                        'L': if (get = 'U') and (get = 'E') then
                               case get of
                                 ' ': result := toVALUE;
                                 'S': if (get = ' ') then result := toVALUES;
                               end;
                        'R': case get of
                               'C': if (get = 'H') and (get = 'A') and (get = 'R') and (get = ' ') then
                                      result := toVARCHAR;
                               'I': if (get = 'A') and (get = 'B') and (get = 'L') and (get = 'E') and (get = ' ') then
                                      result := toVARIABLE;
                               'Y': if (get = 'I') and (get = 'N') and (get = 'G') and (get = ' ') then
                                      result := toVARYING;
                             end;
                      end;
                 'I': if (get = 'E') and (get = 'W') and (get = ' ') then
                        result := toVIEW;
               end;
          'W': case get of
                 'A': if (get = 'I') and (get = 'T') and (get = ' ') then
                        result := toWAIT;
                 'E': if (get = 'E') and (get = 'K') and (get = 'D') and (get = 'A') and
                         (get = 'Y') and (get = ' ') then
                        result := toWEEKDAY;
                 'H': case get of
                        'E': case get of
                               'N': if (get = ' ') then result := toWHEN;
                               'R': if (get = 'E') and (get = ' ') then result := toWHERE;
                             end;
                        'I': if (get = 'L') and (get = 'E') and (get = ' ') then result := toWHILE;
                      end;
                 'I': if (get = 'T') and (get = 'H') and (get = ' ') then result := toWITH;
                 'O': if (get = 'R') and (get = 'K') and (get = ' ') then result := toWORK;
                 'R': if (get = 'I') and (get = 'T') and (get = 'E') and (get = ' ') then result := toWRITE;
               end;
          'Y': if (get = 'E') and (get = 'A') and (get = 'R') then
                 case get of
                   ' ': result := toYEAR;
                   'D': if (get = 'A') and (get = 'Y') and (get = ' ') then
                          result := toYEARDAY;
                 end;
        end;
          if (result = toSymbol) then
          begin
            while (getCM(FCursor^) <> ' ') do inc(FCursor);
            FValue := copy(p, 0, FCursor - p);
          end;
      end;
    // EOL
    #00: if not NextLine then
           exit else
           goto next;
  else
    Error('Unexpected symbol');
  end;
end;

end.
