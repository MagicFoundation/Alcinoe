{ **************************************************************************** }
{                                                                              }
{    Pascal PreProcessor Parser                                                }
{    Copyright (c) 2001 Barry Kelly.                                           }
{    barry_j_kelly@hotmail.com                                                 }
{                                                                              }
{    The contents of this file are subject to the Mozilla Public License       }
{    Version 1.1 (the "License"); you may not use this file except in          }
{    compliance with the License. You may obtain a copy of the License at      }
{    http://www.mozilla.org/MPL/                                               }
{                                                                              }
{    Software distributed under the License is distributed on an "AS IS"       }
{    basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   }
{    License for the specific language governing rights and limitations        }
{    under the License.                                                        }
{                                                                              }
{    The Original Code is PppParser.pas                                        }
{                                                                              }
{    The Initial Developer of the Original Code is Barry Kelly.                }
{    Portions created by Barry Kelly are Copyright (C) 2001                    }
{    Barry Kelly. All Rights Reserved.                                         }
{                                                                              }
{    Contributors:                                                             }
{      Robert Rossmair,                                                        }
{      Peter Thörnqvist,                                                       }
{      Florent Ouchet                                                          }
{                                                                              }
{    Alternatively, the contents of this file may be used under the terms      }
{    of the Lesser GNU Public License (the  "LGPL License"), in which case     }
{    the provisions of LGPL License are applicable instead of those            }
{    above.  If you wish to allow use of your version of this file only        }
{    under the terms of the LPGL License and not to allow others to use        }
{    your version of this file under the MPL, indicate your decision by        }
{    deleting  the provisions above and replace  them with the notice and      }
{    other provisions required by the LGPL License.  If you do not delete      }
{    the provisions above, a recipient may use your version of this file       }
{    under either the MPL or the LPGL License.                                 }
{                                                                              }
{ **************************************************************************** }
{                                                                              }
{ Last modified: $Date::                                                     $ }
{ Revision:      $Rev::                                                      $ }
{ Author:        $Author::                                                   $ }
{                                                                              }
{ **************************************************************************** }

unit JclPreProcessorParser;

{$I jcl.inc}

interface

uses
  {$IFDEF HAS_UNITSCOPE}
  System.SysUtils, System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils, Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase, JclContainerIntf, JclPreProcessorLexer;

type
  TPppState = class;
  
  EPppParserError = class(EJclError);

  TJppParser = class
  private
    FLexer: TJppLexer;
    FState: TPppState;
    FResult: string;
    FResultLen: Integer;
    FLineBreakPos: Integer;
    FAllWhiteSpaceIn: Boolean;
    FAllWhiteSpaceOut: Boolean;
  protected
    procedure AddResult(const S: string; FixIndent: Boolean = False; ForceRecurseTest: Boolean = False);
    function IsExcludedInclude(const FileName: string): Boolean;

    procedure NextToken;

    procedure ParseText;
    procedure ParseCondition(Token: TJppToken);
    function ParseInclude: string;

    procedure ParseDefine(Skip: Boolean);
    procedure ParseUndef(Skip: Boolean);

    procedure ParseDefineMacro;
    procedure ParseExpandMacro;
    procedure ParseUndefMacro;

    procedure ParseGetBoolValue;
    procedure ParseGetIntValue;
    procedure ParseGetStrValue;
    procedure ParseLoop;
    procedure ParseSetBoolValue;
    procedure ParseSetIntValue;
    procedure ParseSetStrValue;
  public
    constructor Create(const ABuffer: string; APppState: TPppState);
    destructor Destroy; override;
    function Parse: string;

    property Lexer: TJppLexer read FLexer;
    property State: TPppState read FState;
  end;

  EPppState = class(EJclError);

  TPppOption = (poProcessIncludes, poProcessDefines, poStripComments,
    poProcessMacros, poProcessValues, poNoWarningHeader, poKeepTabAndSpaces,
    poIgnoreUnterminatedStrings);
  TPppOptions = set of TPppOption;

  TTriState = (ttUnknown, ttUndef, ttDefined);

  TPppStateItem = class
  public
    DefinedKeywords: IJclStrMap;
    ExcludedFiles: IJclStrList;
    Macros: IJclStrIntfMap;
    SearchPath: IJclStrList;
    TriState: TTriState;
  end;

  TPppStateItemClass = class of TPppStateItem;

  TPppProvider = class(TPersistent)
  protected
    function GetBoolValue(const Name: string): Boolean; virtual; abstract;
    function GetDefine(const ASymbol: string): TTriState; virtual; abstract;
    function GetIntegerValue(const Name: string): Integer; virtual; abstract;
    function GetStringValue(const Name: string): string; virtual; abstract;
    procedure SetBoolValue(const Name: string; Value: Boolean); virtual; abstract;
    procedure SetDefine(const ASymbol: string; const Value: TTriState); virtual; abstract;
    procedure SetIntegerValue(const Name: string; Value: Integer); virtual; abstract;
    procedure SetStringValue(const Name, Value: string); virtual; abstract;
  public
    property Defines[const ASymbol: string]: TTriState read GetDefine write SetDefine;
    property BoolValues[const Name: string]: Boolean read GetBoolValue write SetBoolValue;
    property StringValues[const Name: string]: string read GetStringValue write SetStringValue;
    property IntegerValues[const Name: string]: Integer read GetIntegerValue write SetIntegerValue;
  end;

  TPppState = class(TPppProvider)
  private
    FStateStack: IJclStack;
    FOptions: TPppOptions;
    function InternalPeekDefines: IJclStrMap;
    function InternalPeekExcludedFiles: IJclStrList;
    function InternalPeekMacros: IJclStrIntfMap;
    function InternalPeekSearchPath: IJclStrList;
    function InternalPeekTriState: TTriState;
    procedure InternalSetTriState(Value: TTriState);
  protected
    class function StateItemClass: TPppStateItemClass; virtual;
    procedure InternalPushState(FromStateItem, ToStateItem: TPppStateItem); virtual;
    function PeekStateItem: TPppStateItem;

    function GetOptions: TPppOptions;
    procedure SetOptions(AOptions: TPppOptions);

    function FindMacro(const AMacroName: string): IJclStrList;
    function AssociateParameters(const ParamNames: IJclStrList;
      const ParamValues: TDynStringArray): TDynWideStringArray;

    function GetBoolValue(const Name: string): Boolean; override;
    function GetDefine(const ASymbol: string): TTriState; override;
    function GetIntegerValue(const Name: string): Integer; override;
    function GetStringValue(const Name: string): string; override;
    procedure SetBoolValue(const Name: string; Value: Boolean); override;
    procedure SetDefine(const ASymbol: string; const Value: TTriState); override;
    procedure SetIntegerValue(const Name: string; Value: Integer); override;
    procedure SetStringValue(const Name, Value: string); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AfterConstruction; override;
  
    { PushState is called at the start of every unit, and PopState at the
      end. This means that any declarations like $DEFINE will be file-local
      in scope. }
    procedure PushState;
    procedure PopState;

    property TriState: TTriState read InternalPeekTriState write InternalSetTriState;

    procedure Define(const ASymbol: string);
    procedure Undef(const ASymbol: string);

    function FindFile(const AName: string): TStream;
    procedure AddToSearchPath(const AName: string);

    procedure AddFileToExclusionList(const AName: string);
    function IsFileExcluded(const AName: string): Boolean;

    function ExpandMacro(const AName: string; const ParamValues: TDynStringArray): string; virtual;
    procedure DefineMacro(const AName: string; const ParamNames: TDynStringArray;
      const Value: string);
    procedure UndefMacro(const AName: string; const ParamNames: TDynStringArray);

    property Options: TPppOptions read GetOptions write SetOptions;
  end;

  TPppStateClass = class of TPppState;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  System.TypInfo,
  {$ELSE ~HAS_UNITSCOPE}
  TypInfo,
  {$ENDIF ~HAS_UNITSCOPE}
  JclStrings, JclStreams, JclSysUtils, JclArrayLists, JclHashMaps, JclStacks;
  
function AllWhiteSpace(P: PChar; KeepTabAndSpaces: Boolean): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to StrLen(P) do
    case P^ of
      NativeTab, NativeSpace:
        if KeepTabAndSpaces then
        begin
          Result := False;
          Break;
        end
        else
          Inc(P);
      NativeLineFeed, NativeCarriageReturn:
        Inc(P);
    else
      Result := False;
      Break;
    end;
end;

function ParseMacro(const MacroText: string; var MacroName: string; var ParamNames: TDynStringArray;
  ParamDeclaration: Boolean): Integer;
var
  I, J: Integer;
  Comment: Boolean;
  ParenthesisCount: Integer;
  MacroTextLen: Integer;
  MacroParenthesis, MacroBracket: Boolean;
begin
  MacroTextLen := Length(MacroText);
  I := 1;
  while (I <= MacroTextLen) and not CharIsSpace(MacroText[I]) do
    Inc(I);
  while (I <= MacroTextLen) and CharIsSpace(MacroText[I]) do
    Inc(I);
  J := I;
  while (J <= MacroTextLen) and CharIsValidIdentifierLetter(MacroText[J]) do
    Inc(J);
  MacroName := Copy(MacroText, I, J - I);

  if J <= MacroTextLen then
  begin
    SetLength(ParamNames, 0);
    MacroParenthesis := MacroText[J] = '(';
    MacroBracket := MacroText[J] = '[';
    if MacroParenthesis or MacroBracket then
    begin
      Inc(J);
      if ParamDeclaration then
      begin
        repeat
          while (J <= MacroTextLen) and CharIsSpace(MacroText[J]) do
            Inc(J);
          I := J;
          while (I <= MacroTextLen) and CharIsValidIdentifierLetter(MacroText[I]) do
            Inc(I);
          SetLength(ParamNames, Length(ParamNames) + 1);
          ParamNames[High(ParamNames)] := Copy(MacroText, J, I - J);
          while (I <= MacroTextLen) and CharIsSpace(MacroText[I]) do
            Inc(I);
          if (I <= MacroTextLen) then
          begin
            if MacroParenthesis then
              case MacroText[I] of
                ',':
                  Inc(I);
                ')': ;
              else
                raise EPppParserError.CreateFmt('invalid parameter declaration in macro "%s"', [MacroText]);
              end;
            if MacroBracket then
              case MacroText[I] of
                '|':
                  Inc(I);
                ']': ;
              else
                raise EPppParserError.CreateFmt('invalid parameter declaration in macro "%s"', [MacroText]);
              end;
          end;
          J := I;
        until (J > MacroTextLen) or (MacroParenthesis and (MacroText[J] = ')')) or (MacroBracket and (MacroText[J] = ']'));
      end
      else
      begin
        repeat
          I := J;
          Comment := False;
          ParenthesisCount := 0;

          while I <= MacroTextLen do
          begin
            case MacroText[I] of
              NativeSingleQuote:
                Comment := not Comment;
              '(':
                if not Comment then
                  Inc(ParenthesisCount);
              ')':
                begin
                  if MacroParenthesis and (not Comment) and (ParenthesisCount = 0) then
                    Break;
                  if not Comment then
                    Dec(ParenthesisCount);
                end;
              ']':
                if MacroBracket and (not Comment) and (ParenthesisCount = 0) then
                  Break;
              NativeBackslash:
                if (not Comment) and (ParenthesisCount = 0) and (I < MacroTextLen) and (MacroText[i + 1] = NativeComma) then
                  Inc(I);
              NativeComma:
                if MacroParenthesis and (not Comment) and (ParenthesisCount = 0) then
                  Break;
              '|':
                if MacroBracket and (not Comment) and (ParenthesisCount = 0) then
                  Break;
            end;
            Inc(I);
          end;
          SetLength(ParamNames, Length(ParamNames) + 1);
          ParamNames[High(ParamNames)] := Copy(MacroText, J, I - J);
          StrReplace(ParamNames[High(ParamNames)], '\,', ',', [rfReplaceAll]);
          if MacroParenthesis then
          begin
            if (I < MacroTextLen) and (MacroText[I] = ')') then
            begin
              J := I;
              Break;
            end;
            if (I < MacroTextLen) and (MacroText[I] = ',') then
              Inc(I);
          end;
          if MacroBracket then
          begin
            if (I < MacroTextLen) and (MacroText[I] = ']') then
            begin
              J := I;
              Break;
            end;
            if (I < MacroTextLen) and (MacroText[I] = '|') then
              Inc(I);
          end;
          J := I;
        until J > MacroTextLen;
      end;
      if J <= MacroTextLen then
      begin
        if MacroParenthesis and (MacroText[J] = ')') then
          Inc(J) // skip )
        else
        if MacroBracket and (MacroText[J] = ']') then
          Inc(J) // skip ]
        else
          raise EPppParserError.CreateFmt('Unterminated list of arguments for macro "%s"', [MacroText]);
      end;
    end
    else
    begin
      while (J <= MacroTextLen) and CharIsSpace(MacroText[J]) do
        Inc(J);
    end;
  end;
  Result := J;
end;

{ TJppParser }

constructor TJppParser.Create(const ABuffer: string; APppState: TPppState);
begin
  inherited Create;
  Assert(APppState <> nil);

  FLexer := TJppLexer.Create(ABuffer, poIgnoreUnterminatedStrings in APppState.Options);
  FState := APppState;
  FState.Undef('PROTOTYPE');
end;

destructor TJppParser.Destroy;
begin
  FLexer.Free;
  inherited Destroy;
end;

procedure TJppParser.AddResult(const S: string; FixIndent, ForceRecurseTest: Boolean);
var
  I, J: Integer;
  LinePrefix, AResult, Line: string;
  TempMemoryStream: TMemoryStream;
  TempStringStream: TJclAutoStream;
  TempLexer: TJppLexer;
  TempParser: TJppParser;
  Lines: TStrings;
  Recurse: Boolean;
begin
  if State.TriState = ttUndef then
    Exit;

  AResult := S;
  // recurse macro expanding
  if (AResult <> '') and (ForceRecurseTest or (StrIPos('$JPP', AResult) > 0)) then
  begin
    try
      Recurse := False;
      TempLexer := TJppLexer.Create(AResult, poIgnoreUnterminatedStrings in State.Options);
      try
        State.PushState;
        while True do
        begin
          case TempLexer.CurrTok of
            ptEof:
              Break;
            ptDefine,
            ptJppDefine,
            ptUndef,
            ptJppUndef:
              if poProcessDefines in State.Options then
              begin
                Recurse := True;
                Break;
              end;
            ptIfdef, ptIfndef:
              if (poProcessDefines in State.Options) and (State.Defines[TempLexer.TokenAsString] in [ttDefined, ttUndef]) then
              begin
                Recurse := True;
                Break;
              end;
            ptJppDefineMacro,
            ptJppExpandMacro,
            ptJppUndefMacro:
              if poProcessMacros in State.Options then
              begin
                Recurse := True;
                Break;
              end;
            ptJppGetStrValue,
            ptJppGetIntValue,
            ptJppGetBoolValue,
            ptJppSetStrValue,
            ptJppSetIntValue,
            ptJppSetBoolValue,
            ptJppLoop:
              if poProcessValues in State.Options then
              begin
                Recurse := True;
                Break;
              end;
          end;
          TempLexer.NextTok;
        end;
      finally
        State.PopState;
        TempLexer.Free;
      end;
      if Recurse then
      begin
        TempMemoryStream := TMemoryStream.Create;
        try
          TempStringStream := TJclAutoStream.Create(TempMemoryStream);
          try
            TempStringStream.WriteString(AResult, 1, Length(AResult));
            TempStringStream.Seek(0, soBeginning);
            TempParser := TJppParser.Create(TempStringStream.ReadString, State);
            try
              AResult := TempParser.Parse;
            finally
              TempParser.Free;
            end;
          finally
            TempStringStream.Free;
          end;
        finally
          TempMemoryStream.Free;
        end;
      end;
    except
      // The text might not be well-formed Pascal source and
      // thus exceptions might be raised, in such case, just add the text without recursion
      AResult := S;
    end;
  end;
  if FixIndent and (AResult <> '') then
  begin
    // find the number of white space at the beginning of the current line (indentation level)
    I := FResultLen + 1;
    while (I > 1) and not CharIsReturn(FResult[I - 1]) do
     Dec(I);
    J := I;
    while (J <= FResultLen) and CharIsWhiteSpace(FResult[J]) do
      Inc(J);
    LinePrefix := StrRepeat(NativeSpace, J - I);

    Lines := TStringList.Create;
    try
      StrToStrings(AResult, NativeLineBreak, Lines);
      if not (poKeepTabAndSpaces in State.Options) then
      begin
        // remove first empty lines
        while Lines.Count > 0 do
        begin
          if Lines.Strings[0] = '' then
            Lines.Delete(0)
          else
            Break;
        end;
        // remove last empty lines
        for I := Lines.Count - 1 downto 0 do
        begin
          if Lines.Strings[I] = '' then
            Lines.Delete(I)
          else
            Break;
        end;
      end;
      // fix line offsets
      if LinePrefix <> '' then
        for I := 1 to Lines.Count - 1 do
      begin
        Line := Lines.Strings[I];
        if Line <> '' then
          Lines.Strings[I] := LinePrefix + Line;
      end;
      AResult := StringsToStr(Lines, NativeLineBreak);
    finally
      Lines.Free;
    end;
  end;
  if AResult <> '' then
  begin
    while FResultLen + Length(AResult) > Length(FResult) do
      SetLength(FResult, Length(FResult) * 2);
    Move(AResult[1], FResult[FResultLen + 1], Length(AResult) * SizeOf(Char));
    FAllWhiteSpaceOut := FAllWhiteSpaceOut and AllWhiteSpace(PChar(AResult), poKeepTabAndSpaces in State.Options);
    Inc(FResultLen, Length(AResult));
  end;
end;

function TJppParser.IsExcludedInclude(const FileName: string): Boolean;
begin
  Result := State.IsFileExcluded(FileName);
end;

procedure TJppParser.NextToken;
begin
  Lexer.NextTok;

  if State.TriState = ttUndef then
    Exit;
    
  case Lexer.CurrTok of
    ptEof, ptEol:
      // do not change FAllWhiteSpaceIn
      ;
    ptComment:
      FAllWhiteSpaceIn := False;
    ptText:
      FAllWhiteSpaceIn := FAllWhiteSpaceIn and AllWhiteSpace(PChar(Lexer.TokenAsString), poKeepTabAndSpaces in State.Options);
    ptDefine,
    ptUndef,
    ptIfdef,
    ptIfndef,
    ptIfopt,
    ptElse,
    ptEndif,
    ptJppDefine,
    ptJppUndef,
    ptJppDefineMacro,
    ptJppExpandMacro,
    ptJppUndefMacro,
    ptJppGetStrValue,
    ptJppGetIntValue,
    ptJppGetBoolValue,
    ptJppSetStrValue,
    ptJppSetIntValue,
    ptJppSetBoolValue,
    ptJppLoop:
      FAllWhiteSpaceIn := False;
    ptInclude:
      FAllWhiteSpaceIn := IsExcludedInclude(Lexer.TokenAsString);
  else
    // Error
  end;
end;

function TJppParser.Parse: string;
begin
  FLexer.Reset;
  SetLength(FResult, 64 * 1024);
  FillChar(FResult[1], Length(FResult) * SizeOf(Char), 0);
  FResultLen := 0;
  FLineBreakPos := 1;
  FAllWhiteSpaceOut := True;

  ParseText;
  SetLength(FResult, FResultLen);
  Result := FResult;
end;

procedure TJppParser.ParseCondition(Token: TJppToken);
  procedure PushAndExecute(NewTriState: TTriState);
  var
    NeedPush: Boolean;
  begin
    NeedPush := State.TriState <> NewTriState;
    if NeedPush then
      State.PushState;
    try
      State.TriState := NewTriState;
      NextToken;
      ParseText;
    finally
      if NeedPush then
        State.PopState;
    end;
  end;
var
  Condition: string;
  ConditionTriState: TTriState;
begin
  Condition := Lexer.TokenAsString;
  ConditionTriState := State.Defines[Condition];
  // parse the first part of the $IFDEF or $IFNDEF
  case ConditionTriState of
    ttUnknown:
      begin
        State.PushState;
        try
          // preserve the $IFDEF or $IFNDEF
          AddResult(Lexer.RawComment);
          // assume that the symbol is defined in the $IFDEF
          if Token = ptIfdef then
            State.Define(Condition)
          else
          // assume that the symbol is not defined in the $IFNDEF
          if Token = ptIfndef then
            State.Undef(Condition);
          NextToken;
          ParseText;
        finally
          State.PopState;
        end;
      end;
    ttUndef:
      if Token = ptIfdef then
        PushAndExecute(ttUndef)
      else
      if Token = ptIfndef then
        PushAndExecute(ttDefined);
    ttDefined:
      if Token = ptIfdef then
        PushAndExecute(ttDefined)
      else
      if Token = ptIfndef then
        PushAndExecute(ttUndef);
  end;
  // part the second part of the $IFDEF or $IFNDEF if any
  if Lexer.CurrTok = ptElse then
  begin
    case ConditionTriState of
      ttUnknown:
        begin
          State.PushState;
          try
            // preserve the $ELSE
            AddResult(Lexer.RawComment);
            // assume that the symbol is not defined after the $IFDEF
            if Token = ptIfdef then
              State.Undef(Condition)
            else
            // assume that the symbol is defined after the $IFNDEF
            if Token = ptIfndef then
              State.Define(Condition);
            NextToken;
            ParseText;
          finally
            State.PopState;
          end;
        end;
      ttUndef:
        begin
          if Token = ptIfdef then
            PushAndExecute(ttDefined)
          else
          if Token = ptIfndef then
            PushAndExecute(ttUndef);
          //State.Defines[Condition] := ttDefined;
        end;
      ttDefined:
        begin
          if Token = ptIfdef then
            PushAndExecute(ttUndef)
          else
          if Token = ptIfndef then
            PushAndExecute(ttDefined);
          //State.Defines[Condition] := ttUndef;
        end;
      end;
  end;
  if Lexer.CurrTok <> ptEndif then
    Lexer.Error('$ENDIF expected');
  case ConditionTriState of
    ttUnknown:
      // preserve the $ENDIF
      AddResult(Lexer.RawComment);
    ttUndef: ;
    ttDefined: ;
  end;
  NextToken;
end;

procedure TJppParser.ParseDefine(Skip: Boolean);
var
  Condition: string;
begin
  Condition := Lexer.TokenAsString;
  case State.Defines[Condition] of
    // the symbol is not defined
    ttUnknown,
    ttUndef:
      begin
        State.Defines[Lexer.TokenAsString] := ttDefined;
        if not Skip then
          AddResult(Lexer.RawComment);
      end;
    // the symbol is already defined, always skip it
    ttDefined: ;
  end;
  NextToken;
end;

procedure TJppParser.ParseDefineMacro;
var
  I, J: Integer;
  MacroText, MacroName, MacroValue: string;
  ParamNames: TDynStringArray;
begin
  MacroText := Lexer.TokenAsString;
  I := ParseMacro(MacroText, MacroName, ParamNames, True);
  if I <= Length(MacroText) then
  begin
    if Copy(MacroText, I, Length(NativeLineBreak)) = NativeLineBreak then
      Inc(I, Length(NativeLineBreak));
    J := Length(MacroText);
    if MacroText[J] = ')' then
      Dec(J);
    MacroValue := Copy(MacroText, I, J - I);
    State.DefineMacro(MacroName, ParamNames, MacroValue);
  end;
  NextToken;
end;

procedure TJppParser.ParseExpandMacro;
var
  MacroText, MacroName, AResult: string;
  ParamNames: TDynStringArray;
begin
  MacroText := Lexer.TokenAsString;
  ParseMacro(MacroText, MacroName, ParamNames, False);
  // macros are expanded in a sub-state
  State.PushState;
  try
    AResult := State.ExpandMacro(MacroName, ParamNames);
    // add result to buffer
    AddResult(AResult, True, True);
  finally
    State.PopState;
  end;
  NextToken;
end;

procedure TJppParser.ParseUndef(Skip: Boolean);
var
  Condition: string;
begin
  Condition := Lexer.TokenAsString;
  case State.Defines[Condition] of
    // the symbol is not defined
    ttUnknown,
    ttDefined:
      begin
        State.Defines[Lexer.TokenAsString] := ttUndef;
        if not Skip then
          AddResult(Lexer.RawComment);
      end;
    // the symbol is already defined, skip it
    ttUndef: ;
  end;
  NextToken;
end;

procedure TJppParser.ParseUndefMacro;
var
  MacroText, MacroName: string;
  ParamNames: TDynStringArray;
begin
  MacroText := Lexer.TokenAsString;
  ParseMacro(MacroText, MacroName, ParamNames, True);
  State.UndefMacro(MacroName, ParamNames);
  NextToken;
end;

function TJppParser.ParseInclude: string;
var
  oldLexer, newLexer: TJppLexer;
  fsIn: TStream;
  ssIn: TJclAutoStream;
begin
  Result := '';
  Assert(Lexer.TokenAsString <> '');
  { we must prevent case of $I- & $I+ becoming file names }
  if   (Lexer.TokenAsString[1] = '-')
    or (Lexer.TokenAsString[1] = '+')
    or IsExcludedInclude(Lexer.TokenAsString) then
    Result := Lexer.RawComment
  else
  begin
    fsIn := nil;
    ssIn := nil;
    newLexer := nil;

    oldLexer := Lexer;
    try
      try
        fsIn := FState.FindFile(Lexer.TokenAsString);
      except
        on e: Exception do
          Lexer.Error(e.Message);
      end;
      ssIn := TJclAutoStream.Create(fsIn);
      newLexer := TJppLexer.Create(ssIn.ReadString, poIgnoreUnterminatedStrings in State.Options);
      FLexer := newLexer;
      ParseText;
    finally
      FLexer := oldLexer;
      ssIn.Free;
      fsIn.Free;
      newLexer.Free;
    end;
  end;
  NextToken;
end;

procedure TJppParser.ParseGetStrValue;
var
  Name: string;
begin
  Name := Lexer.TokenAsString;
  AddResult(State.StringValues[Name]);
  NextToken;
end;

procedure TJppParser.ParseGetIntValue;
var
  Name: string;
begin
  Name := Lexer.TokenAsString;
  AddResult(IntToStr(State.IntegerValues[Name]));
  NextToken;
end;

procedure TJppParser.ParseGetBoolValue;
var
  Name: string;
begin
  Name := Lexer.TokenAsString;
  AddResult(BoolToStr(State.BoolValues[Name], True));
  NextToken;
end;

procedure TJppParser.ParseLoop;
var
  I, J, RepeatIndex, RepeatCount: Integer;
  RepeatText, IndexName, CountName: string;
begin
  I := 1;
  RepeatText := Lexer.RawComment;
  while (I <= Length(RepeatText)) and not CharIsWhiteSpace(RepeatText[I]) do
    Inc(I);
  while (I <= Length(RepeatText)) and CharIsWhiteSpace(RepeatText[I]) do
    Inc(I);
  J := I;
  while (J <= Length(RepeatText)) and CharIsValidIdentifierLetter(RepeatText[J]) do
    Inc(J);
  IndexName := Copy(RepeatText, I, J - I);
  while (J <= Length(RepeatText)) and CharIsWhiteSpace(RepeatText[J]) do
    Inc(J);
  I := J;
  while (J <= Length(RepeatText)) and CharIsValidIdentifierLetter(RepeatText[I]) do
    Inc(I);
  CountName := Copy(RepeatText, J, I - J);

  J := Length(RepeatText);
  if RepeatText[J] = ')' then
    Dec(J);
  RepeatText := Copy(RepeatText, I, J - I);
  RepeatCount := State.IntegerValues[CountName];
  for RepeatIndex := 0 to RepeatCount - 1 do
  begin
    State.IntegerValues[IndexName] := RepeatIndex;
    AddResult(RepeatText);
  end;
  State.IntegerValues[IndexName] := -1;
  NextToken;
end;

procedure TJppParser.ParseSetStrValue;
var
  I, J: Integer;
  Text, Name, Value: string;
begin
  I := 1;
  Text := Lexer.RawComment;
  while (I <= Length(Text)) and not CharIsWhiteSpace(Text[I]) do
    Inc(I);
  while (I <= Length(Text)) and CharIsWhiteSpace(Text[I]) do
    Inc(I);
  J := I;
  while (J <= Length(Text)) and CharIsValidIdentifierLetter(Text[J]) do
    Inc(J);
  Name := Copy(Text, I, J - I);
  while (J <= Length(Text)) and CharIsWhiteSpace(Text[J]) do
    Inc(J);
  I := Length(Text);
  if Text[I] = ')' then
    Dec(I);
  Value := Copy(Text, J, I - J);
  State.StringValues[Name] := Value;
  NextToken;
end;

procedure TJppParser.ParseSetIntValue;
var
  I, J: Integer;
  Text, Name, Value: string;
begin
  I := 1;
  Text := Lexer.RawComment;
  while (I <= Length(Text)) and not CharIsWhiteSpace(Text[I]) do
    Inc(I);
  while (I <= Length(Text)) and CharIsWhiteSpace(Text[I]) do
    Inc(I);
  J := I;
  while (J <= Length(Text)) and CharIsValidIdentifierLetter(Text[J]) do
    Inc(J);
  Name := Copy(Text, I, J - I);
  while (J <= Length(Text)) and CharIsWhiteSpace(Text[J]) do
    Inc(J);
  I := Length(Text);
  if Text[I] = ')' then
    Dec(I);
  Value := Copy(Text, J, I - J);
  State.IntegerValues[Name] := StrToInt(Value);
  NextToken;
end;

procedure TJppParser.ParseSetBoolValue;
var
  I, J: Integer;
  Text, Name, Value: string;
begin
  I := 1;
  Text := Lexer.RawComment;
  while (I <= Length(Text)) and not CharIsWhiteSpace(Text[I]) do
    Inc(I);
  while (I <= Length(Text)) and CharIsWhiteSpace(Text[I]) do
    Inc(I);
  J := I;
  while (J <= Length(Text)) and CharIsValidIdentifierLetter(Text[J]) do
    Inc(J);
  Name := Copy(Text, I, J - I);
  while (J <= Length(Text)) and CharIsWhiteSpace(Text[J]) do
    Inc(J);
  I := Length(Text);
  if Text[I] = ')' then
    Dec(I);
  Value := Copy(Text, J, I - J);
  State.BoolValues[Name] := StrToBoolean(Value);
  NextToken;
end;

procedure TJppParser.ParseText;

  procedure AddRawComment;
  begin
    AddResult(Lexer.RawComment);
    NextToken;
  end;

  procedure DeleteCurrentLineIfOrphaned;
  begin
    if not FAllWhiteSpaceIn and FAllWhiteSpaceOut then
      if FLineBreakPos <= FResultLen then
      begin
        FResultLen := FLineBreakPos - 1;
        FResult[FResultLen + 1] := #0;
      end;
  end;

begin
  while True do
    case Lexer.CurrTok of
      ptComment:
        begin
          if not (poStripComments in State.Options) then
            AddResult(Lexer.TokenAsString);
          NextToken;
        end;

      ptEof:
        begin
          DeleteCurrentLineIfOrphaned;
          Break;
        end;

      ptEol:
        begin
          AddResult(Lexer.TokenAsString);
          DeleteCurrentLineIfOrphaned;
          FLineBreakPos := FResultLen + 1;
          FAllWhiteSpaceIn := True;
          FAllWhiteSpaceOut := True;
          NextToken;
        end;

      ptText:
      begin
        AddResult(Lexer.TokenAsString);
        NextToken;
      end;

      ptDefine, ptJppDefine, ptUndef, ptJppUndef, ptIfdef, ptIfndef, ptIfopt:
        if poProcessDefines in State.Options then
          case Lexer.CurrTok of
            ptDefine:
              ParseDefine(False);
            ptJppDefine:
              ParseDefine(True);
            ptUndef:
              ParseUndef(False);
            ptJppUndef:
              ParseUndef(True);
            ptIfdef:
              ParseCondition(ptIfdef);
            ptIfndef:
              ParseCondition(ptIfndef);
            ptIfopt:
              ParseCondition(ptIfopt);
          end
        else
          AddRawComment;

      ptElse, ptEndif:
        if poProcessDefines in State.Options then
          Break
        else
          AddRawComment;

      ptInclude:
        if poProcessIncludes in State.Options then
          AddResult(ParseInclude)
        else
          AddRawComment;

      ptJppDefineMacro, ptJppExpandMacro, ptJppUndefMacro:
        if State.TriState = ttUndef then
          NextToken
        else
        if poProcessMacros in State.Options then
          case Lexer.CurrTok of
            ptJppDefineMacro:
              ParseDefineMacro;
            ptJppExpandMacro:
              ParseExpandMacro;
            ptJppUndefMacro:
              ParseUndefMacro;
          end
        else
          AddRawComment;

      ptJppGetStrValue,
      ptJppGetIntValue,
      ptJppGetBoolValue,
      ptJppSetStrValue,
      ptJppSetIntValue,
      ptJppSetBoolValue,
      ptJppLoop:
        if State.TriState = ttUndef then
          NextToken
        else
        if poProcessValues in State.Options then
          case Lexer.CurrTok of
            ptJppGetStrValue:
              ParseGetStrValue;
            ptJppGetIntValue:
              ParseGetIntValue;
            ptJppGetBoolValue:
              ParseGetBoolValue;
            ptJppSetStrValue:
              ParseSetStrValue;
            ptJppSetIntValue:
              ParseSetIntValue;
            ptJppSetBoolValue:
              ParseSetBoolValue;
            ptJppLoop:
              ParseLoop;
          end
        else
          AddRawComment;
    else
      Break;
    end;
end;

//=== { TPppState } ==========================================================

constructor TPppState.Create;
begin
  inherited Create;
  FStateStack := TJclStack.Create(16, True);
end;

destructor TPppState.Destroy;
begin
  FStateStack := nil;
  inherited Destroy;
end;

procedure TPppState.AddFileToExclusionList(const AName: string);
begin
  InternalPeekExcludedFiles.Add(AName);
end;

procedure TPppState.AddToSearchPath(const AName: string);
begin
  InternalPeekSearchPath.Add(AName);
end;

procedure TPppState.AfterConstruction;
var
  StateItem: TPppStateItem;
begin
  StateItem := StateItemClass.Create;
  InternalPushState(nil, StateItem);
  FStateStack.Push(StateItem);
end;

function TPppState.AssociateParameters(const ParamNames: IJclStrList;
  const ParamValues: TDynStringArray): TDynWideStringArray;
var
  StrParams: TStrings;
  AssociationByName: Boolean;
  Index, ParamIndex: Integer;
  AParamName, AParamText: string;
begin
  SetLength(Result, Length(ParamValues));
  AssociationByName := True;
  StrParams := TStringList.Create;
  try
    for Index := Low(ParamValues) to High(ParamValues) do
    begin
      StrParams.Add(ParamValues[Index]);
      AParamName := StrParams.Names[Index];
      if Assigned(ParamNames) and (AParamName <> '') then
      begin
        // verify parameter names
        ParamIndex := ParamNames.IndexOf(AParamName);
        if ParamIndex < 0 then
          AssociationByName := False;
      end
      else
        AssociationByName := False;
    end;
    for Index := Low(ParamValues) to High(ParamValues) do
    begin
      if AssociationByName then
        AParamText := StrParams.Values[ParamNames.Strings[Index]]
      else
        AParamText := StrParams.Strings[Index];
      Result[Index] := WideString(AParamText);
    end;
  finally
    StrParams.Free;
  end;
end;

function TPppState.ExpandMacro(const AName: string;
  const ParamValues: TDynStringArray): string;
var
  AMacro: IJclStrList;
  AMacroName, AMacroText: string;
  Index: Integer;
  Params: array of TVarRec;
  AMacroParams: TDynWideStringArray;
begin
  AMacroName := Format('%s`%d', [AName, Length(ParamValues)]);
  AMacro := FindMacro(AMacroName);
  // the macro text is the last item, previous items are the macro parameter names
  AMacroText := AMacro.Strings[AMacro.Size - 1];
  AMacroParams := AssociateParameters(AMacro.SubList(0, AMacro.Size - 1), ParamValues);

  SetLength(Params, Length(ParamValues));
  for Index := Low(ParamValues) to High(ParamValues) do
  begin
    Params[Index].VType := vtPWideChar;
    Params[Index].VPWideChar := PWideChar(AMacroParams[Index]);
  end;
  Result := Format(AMacroText, Params);
end;

procedure TPppState.Define(const ASymbol: string);
begin
  Defines[ASymbol] := ttDefined;
end;

procedure TPppState.DefineMacro(const AName: string;
  const ParamNames: TDynStringArray; const Value: string);
var
  AMacro: IJclStrList;
  AMacros: IJclStrIntfMap;
  AMacroNames: IJclStrIterator;
  AMacroName, AMacroFormat: string;
  Index: Integer;
begin
  AMacros := InternalPeekMacros;
  AMacroName := Format('%s`%d', [AName, Length(ParamNames)]);
  AMacroNames := AMacros.KeySet.First;
  while AMacroNames.HasNext do
    if JclStrings.StrSame(AMacroNames.Next, AMacroName) then
      raise EPppState.CreateFmt('macro "%s" is already defined', [AName]);
  AMacroFormat := Value;
  AMacro := TJclStrArrayList.Create(16);
  for Index := Low(ParamNames) to High(ParamNames) do
  begin
    StrReplace(AMacroFormat, ParamNames[Index], '%' + IntToStr(Index) + ':s', [rfReplaceAll, rfIgnoreCase]);
    // the first elements in the list are the macro parameter names
    AMacro.Add(ParamNames[Index]);
  end;
  // the macro text is the last element in the list
  AMacro.Add(AMacroFormat);
  AMacros.Items[AMacroName] := AMacro;
end;

function TPppState.FindFile(const AName: string): TStream;
var
  i: Integer;
  fn: string;
  Found: Boolean;
  ASearchPath: IJclStrList;
begin
  ASearchPath := InternalPeekSearchPath;
  fn := AName;
  Found := FileExists(fn);
  if not Found then
    for i := 0 to ASearchPath.Size - 1 do
    begin
      fn := ASearchPath.Strings[i] + PathDelim + AName;
      if FileExists(fn) then
      begin
        Found := True;
        Break;
      end;
    end;
  if not Found then
    raise EPppState.CreateFmt('File not found: %s', [AName]);
  Result := TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
end;

function TPppState.FindMacro(const AMacroName: string): IJclStrList;
var
  AMacros: IJclStrIntfMap;
  AMacroNames: IJclStrIterator;
begin
  AMacros := InternalPeekMacros;
  AMacroNames := AMacros.KeySet.First;
  while AMacroNames.HasNext do
  begin
    if JclStrings.StrSame(AMacroNames.Next, AMacroName) then
    begin
      Result := AMacros.Items[AMacroNames.GetString] as IJclStrList;
      Exit;
    end;
  end;
  raise EPppState.CreateFmt('unknown macro "%s"', [AMacroName]);
end;

function TPppState.GetBoolValue(const Name: string): Boolean;
var
  VariantValue: Variant;
begin
  VariantValue := GetPropValue(Self, Name);
  Result := Boolean(VariantValue);
end;

function TPppState.GetIntegerValue(const Name: string): Integer;
var
  VariantValue: Variant;
begin
  VariantValue := GetPropValue(Self, Name);
  Result := Integer(VariantValue);
end;

function TPppState.GetStringValue(const Name: string): string;
var
  VariantValue: Variant;
begin
  VariantValue := GetPropValue(Self, Name, True);
  Result := string(VariantValue);
end;

function TPppState.GetOptions: TPppOptions;
begin
  Result := FOptions;
end;

function TPppState.GetDefine(const ASymbol: string): TTriState;
var
  ADefines: IJclStrMap;
  ASymbolNames: IJclStrIterator;
  PI: PPropInfo;
  PV: Variant;
begin
  Result := ttUnknown;
  ADefines := InternalPeekDefines;
  ASymbolNames := ADefines.KeySet.First;
  while ASymbolNames.HasNext do
  begin
    if JclStrings.StrSame(ASymbolNames.Next, ASymbol) then
    begin
      Result := TTriState(ADefines.Items[ASymbolNames.GetString]);
      Break;
    end;
  end;
  if Result = ttUnknown then
  begin
    PI := GetPropInfo(Self, ASymbol);
    if Assigned(PI) then
    begin
      {$IFDEF COMPILER8_UP}
      PV := GetPropValue(Self, PI);
      {$ELSE ~COMPILER8_UP}
      PV := GetPropValue(Self, PI^.Name);
      {$ENDIF ~COMPILER8_UP}
      if Boolean(PV) then
        Result := ttDefined
      else
        Result := ttUndef;
    end;
  end;
end;

function TPppState.InternalPeekDefines: IJclStrMap;
begin
  Result := PeekStateItem.DefinedKeywords;
end;

function TPppState.InternalPeekExcludedFiles: IJclStrList;
begin
  Result := PeekStateItem.ExcludedFiles;
end;

function TPppState.InternalPeekMacros: IJclStrIntfMap;
begin
  Result := PeekStateItem.Macros;
end;

function TPppState.InternalPeekSearchPath: IJclStrList;
begin
  Result := PeekStateItem.SearchPath;
end;

function TPppState.InternalPeekTriState: TTriState;
begin
 Result := PeekStateItem.TriState;
end;

procedure TPppState.InternalPushState(FromStateItem, ToStateItem: TPppStateItem);
begin
  if Assigned(FromStateItem) then
  begin
    // clone
    ToStateItem.DefinedKeywords := (FromStateItem.DefinedKeywords as IJclIntfCloneable).IntfClone as IJclStrMap;
    ToStateItem.ExcludedFiles := (FromStateItem.ExcludedFiles as IJclIntfCloneable).IntfClone as IJclStrList;
    ToStateItem.Macros := (FromStateItem.Macros as IJclIntfCloneable).IntfClone as IJclStrIntfMap;
    ToStateItem.SearchPath := (FromStateItem.SearchPath as IJclIntfCloneable).IntfClone as IJclStrList;
    ToStateItem.TriState := FromStateItem.TriState;
  end
  else
  begin
    // create the first item
    ToStateItem.DefinedKeywords := TJclStrHashMap.Create(16, False);
    ToStateItem.ExcludedFiles := TJclStrArrayList.Create(16);
    ToStateItem.Macros := TJclStrIntfHashMap.Create(16);
    ToStateItem.SearchPath := TJclStrArrayList.Create(16);
    ToStateItem.TriState := ttDefined;
  end;
end;

procedure TPppState.InternalSetTriState(Value: TTriState);
var
  APppStateItem: TPppStateItem;
begin
  APppStateItem := PeekStateItem;
  if APppStateItem.TriState <> ttUndef then
    APppStateItem.TriState := Value;
end;

function TPppState.IsFileExcluded(const AName: string): Boolean;
var
  AExcludedFiles: IJclStrList;
  AFileNames: IJclStrIterator;
begin
  AExcludedFiles := InternalPeekExcludedFiles;
  AFileNames := AExcludedFiles.First;
  Result := False;
  while AFileNames.HasNext do
  begin
    if JclStrings.StrSame(AFileNames.Next, AName) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TPppState.PeekStateItem: TPppStateItem;
begin
  if FStateStack.Empty then
    raise EPppState.Create('Internal error: PPP State stack is empty');
  Result := FStateStack.Peek as TPppStateItem; 
end;

procedure TPppState.PopState;
begin
  if FStateStack.Size <= 1 then
    raise EPppState.Create('Internal error: PPP State stack underflow');
  FStateStack.Pop.Free;
end;

procedure TPppState.PushState;
var
  FromStateItem, ToStateItem: TPppStateItem;
begin
  FromStateItem := PeekStateItem;
  ToStateItem := StateItemClass.Create;
  InternalPushState(FromStateItem, ToStateItem);
  FStateStack.Push(ToStateItem);
end;

procedure TPppState.SetOptions(AOptions: TPppOptions);
begin
  FOptions := AOptions;
end;

procedure TPppState.SetBoolValue(const Name: string; Value: Boolean);
var
  VariantValue: Variant;
begin
  VariantValue := Value;
  SetPropValue(Self, Name, VariantValue);
end;

procedure TPppState.SetDefine(const ASymbol: string;
  const Value: TTriState);
var
  ADefines: IJclStrMap;
  ASymbolNames: IJclStrIterator;
  PI: PPropInfo;
begin
  ADefines := InternalPeekDefines;
  ASymbolNames := ADefines.KeySet.First;
  while ASymbolNames.HasNext do
  begin
    if JclStrings.StrSame(ASymbolNames.Next, ASymbol) then
    begin
      ADefines.Items[ASymbolNames.GetString] := TObject(Value);
      Exit;
    end;
  end;
  if Value <> ttUnknown then
  begin
    PI := GetPropInfo(Self, ASymbol);
    if Assigned(PI) then
    begin
      if Value = ttDefined then
        {$IFDEF COMPILER8_UP}
        SetPropValue(Self, PI, True)
        {$ELSE ~COMPILER8_UP}
        SetPropValue(Self, PI^.Name, True)
        {$ENDIF ~COMPILER8_UP}
      else
        {$IFDEF COMPILER8_UP}
        SetPropValue(Self, PI, False);
        {$ELSE ~COMPILER8_UP}
        SetPropValue(Self, PI^.Name, False);
        {$ENDIF ~COMPILER8_UP}
      Exit;
    end;
  end;
  ADefines.Items[ASymbol] := TObject(Value);
end;

procedure TPppState.SetIntegerValue(const Name: string; Value: Integer);
var
  VariantValue: Variant;
begin
  VariantValue := Value;
  SetPropValue(Self, Name, VariantValue);
end;

procedure TPppState.SetStringValue(const Name, Value: string);
var
  VariantValue: Variant;
begin
  VariantValue := Value;
  SetPropValue(Self, Name, VariantValue);
end;

class function TPppState.StateItemClass: TPppStateItemClass;
begin
  Result := TPppStateItem;
end;

procedure TPppState.Undef(const ASymbol: string);
begin
  Defines[ASymbol] := ttUndef;
end;

procedure TPppState.UndefMacro(const AName: string; const ParamNames: TDynStringArray);
var
  AMacros: IJclStrIntfMap;
  AMacroNames: IJclStrIterator;
  AMacroName: string;
begin
  AMacros := InternalPeekMacros;
  AMacroName := Format('%s`%d', [AName, Length(ParamNames)]);
  AMacroNames := AMacros.KeySet.First;
  while AMacroNames.HasNext do
    if JclStrings.StrSame(AMacroNames.Next, AMacroName) then
      AMacros.Remove(AMacroNames.GetString);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
