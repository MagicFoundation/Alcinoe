{---------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License Version
1.1 (the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at
http://www.mozilla.org/NPL/NPL-1_1Final.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: mwSimplePasPar.pas, released November 14, 1999.

The Initial Developer of the Original Code is Martin Waldenburg
(Martin.Waldenburg@T-Online.de).
Portions created by Martin Waldenburg are Copyright (C) 1998, 1999 Martin
Waldenburg.
All Rights Reserved.
Portions CopyRight by Robert Zierer.

Contributor(s):  Vladimir Churbanov, Dean Hill, James Jacobson, LaKraven Studios Ltd, Roman Yankovsky
(This list is ALPHABETICAL)

Last Modified: 2014/09/14
Current Version: 1.10

Notes: This program is an early beginning of a Pascal parser.
I'd like to invite the Delphi community to develop it further and to create
a fully featured Object Pascal parser.

Modification history:

LaKraven Studios Ltd, January 2015:

- Cleaned up version-specifics up to XE8
- Fixed all warnings & hints

Jacob Thurman between 20040301 and 20020401

Made ready for Delphi 8:

Added new directives and keywords: static, sealed, final, operator, unsafe.

Added parsing for custom attributes (based on ECMA C# specification).

Added support for nested types in class declarations.

Jeff Rafter between 20020116 and 20020302

Added AncestorId and AncestorIdList back in, but now treat them as Qualified
Identifiers per Daniel Rolf's fix. The separation from QualifiedIdentifierList
is need for descendent classes.

Added VarName and VarNameList back in for descendent classes, fixed to correctly
use Identifiers as in Daniel's verison

Removed fInJunk flags (they were never used, only set)

Pruned uses clause to remove windows dependency. This required changing
"TPoint" to "TTokenPoint". TTokenPoint was declared in mwPasLexTypes

Daniel Rolf between 20010723 and 20020116

Made ready for Delphi 6

ciClassClass for "class function" etc.
ciClassTypeEnd marks end of a class declaration (I needed that for the delphi-objectif-connector)
ciEnumeratedTypeItem for items of enumerations
ciDirectiveXXX for the platform, deprecated, varargs, local
ciForwardDeclaration for "forward" (until now it has been read but no event)
ciIndexSpecifier for properties
ciObjectTypeEnd marks end of an object declaration
ciObjectProperty property for objects
ciObjectPropertySpecifiers property for objects
ciPropertyDefault marking default of property
ciDispIDSpecifier for dispid

patched some functions for implementing the above things and patching the following bugs/improv.:

ObjectProperty handling overriden properties
ProgramFile, UnitFile getting Identifier instead of dropping it
InterfaceHeritage: Qualified identifiers
bugs in variant records
typedconstant failed with complex set constants. simple patch using ConstantExpression

German localization for the two string constants. Define GERMAN for german string constants.

Greg Chapman on 20010522
Better handling of defaut array property
Separate handling of X and Y in property Pixels[X, Y: Integer through identifier "event"
corrected spelling of "ForwardDeclaration"

James Jacobson on 20010223
semi colon before finalization fix

James Jacobson on 20010223
RecordConstant Fix

Martin waldenburg on 2000107
Even Faster lexer implementation !!!!

James Jacobson on 20010107
  Improper handling of the construct
      property TheName: Integer read FTheRecord.One.Two; (stop at second point)
      where one and two are "qualifiable" structures.

James Jacobson on 20001221
   Stops at the second const.
   property Anchor[const Section: string; const Ident:string]: string read
   changed TmwSimplePasPar.PropertyParameterList

On behalf of  Martin Waldenburg and James Jacobson
 Correction in array property Handling (Matin and James) 07/12/2000
 Use of ExId instead of TokenId in ExportsElements (James) 07/12/2000
 Reverting to old behavior in Statementlist [PtintegerConst put back in] (James) 07/12/2000

Xavier Masson InnerCircleProject : XM : 08/11/2000
  Integration of the new version delivered by Martin Waldenburg with the modification I made described just below

Xavier Masson InnerCircleProject : XM : 07/15/2000
  Added "states/events " for      spaces( SkipSpace;) CRLFco (SkipCRLFco) and
    CRLF (SkipCRLF) this way the parser can give a complete view on code allowing
    "perfect" code reconstruction.
    (I fully now that this is not what a standard parser will do but I think it is more usefull this way ;) )
    go to www.innercircleproject.com for more explanations or express your critisism ;)

previous modifications not logged sorry ;)

Known Issues:
-----------------------------------------------------------------------------}
{----------------------------------------------------------------------------
 Last Modified: 05/22/2001
 Current Version: 1.1
 official version
   Maintained by InnerCircle

   http://www.innercircleproject.org

 02/07/2001
   added property handling in Object types
   changed handling of forward declarations in ExportedHeading method
-----------------------------------------------------------------------------}
unit SimpleParser;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  SysUtils,
  Classes,
  SimpleParser.Lexer.Types,
  SimpleParser.Lexer,
  SimpleParser.Types;

{$INCLUDE SimpleParser.inc}

resourcestring
  rsExpected = '''%s'' expected found ''%s''';
  rsEndOfFile = 'end of file';

const
  ClassMethodDirectiveEnum = [
    ptAbstract,
    ptCdecl,
    ptDynamic,
    ptMessage,
    ptOverride,
    ptOverload,
    ptPascal,
    ptRegister,
    ptReintroduce,
    ptSafeCall,
    ptStdCall,
    ptVirtual,
    ptDeprecated,
    ptLibrary,
    ptPlatform,
    ptStatic,
    ptInline,
    ptFinal,
    ptExperimental,
    ptDispId,
    ptNoreturn
  ];

type
  ESyntaxError = class(Exception)
  private
    FPosXY: TTokenPoint;
  public
    constructor Create(const Msg: string);
    constructor CreateFmt(const Msg: string; const Args: array of const);
    constructor CreatePos(const Msg: string; aPosXY: TTokenPoint);
    property PosXY: TTokenPoint read FPosXY write FPosXY;
  end;

  TmwSimplePasPar = class(TObject)
  private
    FOnMessage: TMessageEvent;
    FLexer: TmwPasLex;
    FInterfaceOnly: Boolean;
    FLastNoJunkPos: Integer;
    FLastNoJunkLen: Integer;
    AheadParse: TmwSimplePasPar;
    FInRound: Integer;
    procedure InitAhead;
    procedure VariableTail;
    function GetInRound: Boolean;
    function GetUseDefines: Boolean;
    function GetScopedEnums: Boolean;
    procedure SetUseDefines(const Value: Boolean);
    procedure SetIncludeHandler(IncludeHandler: IIncludeHandler);
    function GetOnComment: TCommentEvent;
    procedure SetOnComment(const Value: TCommentEvent);
  protected
    procedure Expected(Sym: TptTokenKind); virtual;
    procedure ExpectedEx(Sym: TptTokenKind); virtual;
    procedure ExpectedFatal(Sym: TptTokenKind); virtual;
    procedure HandlePtCompDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtDefineDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtElseDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtEndIfDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfDefDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfNDefDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfOptDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtResourceDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtUndefDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfEndDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtElseIfDirect(Sender: TmwBasePasLex); virtual;
    procedure NextToken; virtual;
    procedure SkipJunk; virtual;
    procedure Semicolon; virtual;
    function GetExID: TptTokenKind; virtual;
    function GetTokenID: TptTokenKind; virtual;
    function GetGenID: TptTokenKind; virtual;
    procedure AccessSpecifier; virtual;
    procedure AdditiveOperator; virtual;
    procedure AddressOp; virtual;
    procedure AlignmentParameter; virtual;
    procedure AsOp; virtual;
    procedure AncestorIdList; virtual;
    procedure AncestorId; virtual;
    procedure AnonymousMethod; virtual;
    procedure AnonymousMethodType; virtual;
    procedure ArrayConstant; virtual;
    procedure ArrayBounds; virtual;
    procedure ArrayDimension; virtual;
    procedure ArrayType; virtual;
    procedure AsmStatement; virtual;
    procedure AssignOp; virtual;
    procedure AtExpression; virtual;
    procedure Block; virtual;
    procedure CaseElseStatement; virtual;
    procedure CaseLabel; virtual;
    procedure CaseLabelList; virtual;
    procedure CaseSelector; virtual;
    procedure CaseStatement; virtual;
    procedure CharString; virtual;
    procedure ClassField; virtual;
    procedure ClassForward; virtual;
    procedure ClassFunctionHeading; virtual;
    procedure ClassHelper; virtual;
    procedure ClassHeritage; virtual;
    procedure ClassMemberList; virtual;
    procedure ClassMethodDirective; virtual;
    procedure ClassMethodHeading; virtual;
    procedure ClassMethodOrProperty; virtual;
    procedure ClassMethodResolution; virtual;
    procedure ClassOperatorHeading; virtual;
    procedure ClassProcedureHeading; virtual;
    procedure ClassClass; virtual;
    procedure ClassConstraint; virtual;
    procedure ClassMethod; virtual;
    procedure ClassProperty; virtual;
    procedure ClassReferenceType; virtual;
    procedure ClassType; virtual;
    procedure ClassTypeEnd; virtual;
    procedure ClassVisibility; virtual;
    procedure CompoundStatement; virtual;
    procedure ConstantColon; virtual;
    procedure ConstantDeclaration; virtual;
    procedure ConstantEqual; virtual;
    procedure ConstantExpression; virtual;
    procedure ConstantName; virtual;
    procedure ConstantType; virtual;
    procedure ConstantValue; virtual;
    procedure ConstantValueTyped; virtual;
    procedure ConstParameter; virtual;
    procedure ConstructorConstraint; virtual;
    procedure ConstructorHeading; virtual;
    procedure ConstructorName; virtual;
    procedure ConstSection; virtual;
    procedure ContainsClause; virtual;
    procedure CustomAttribute; virtual;
    procedure DeclarationSection; virtual;
    procedure DeclarationSections; virtual;
    procedure Designator; virtual;
    procedure DestructorHeading; virtual;
    procedure DestructorName; virtual;
    procedure Directive16Bit; virtual;
    procedure DirectiveBinding; virtual;
    procedure DirectiveBindingMessage; virtual;
    procedure DirectiveCalling; virtual;
    procedure DirectiveDeprecated; virtual;
    procedure DirectiveInline; virtual;
    procedure DirectiveLibrary; virtual;
    procedure DirectiveLocal; virtual;
    procedure DirectivePlatform; virtual;
    procedure DirectiveVarargs; virtual;
    procedure DispInterfaceForward; virtual;
    procedure DispIDSpecifier; virtual;
    procedure DotOp; virtual;
    procedure ElseStatement; virtual;
    procedure ElseExpression; virtual;
    procedure EmptyStatement; virtual;
    procedure EnumeratedType; virtual;
    procedure EnumeratedTypeItem; virtual;
    procedure ExceptBlock; virtual;
    procedure ExceptionBlockElseBranch; virtual;
    procedure ExceptionClassTypeIdentifier; virtual;
    procedure ExceptionHandler; virtual;
    procedure ExceptionHandlerList; virtual;
    procedure ExceptionIdentifier; virtual;
    procedure ExceptionVariable; virtual;
    procedure ExplicitType; virtual;
    procedure ExportedHeading; virtual;
    procedure ExportsClause; virtual;
    procedure ExportsElement; virtual;
    procedure ExportsName; virtual;
    procedure ExportsNameId; virtual;
    procedure Expression; virtual;
    procedure ExpressionList; virtual;
    procedure ExternalDirective; virtual;
    procedure ExternalDirectiveThree; virtual;
    procedure ExternalDirectiveTwo; virtual;
    procedure Factor; virtual;
    procedure FieldDeclaration; virtual;
    procedure FieldList; virtual;
    procedure FieldNameList; virtual;
    procedure FieldName; virtual;
    procedure FileType; virtual;
    procedure FinalizationSection; virtual;
    procedure FinallyBlock; virtual;
    procedure FormalParameterList; virtual;
    procedure FormalParameterSection; virtual;
    procedure ForStatement; virtual;
    procedure ForStatementDownTo; virtual;
    procedure ForStatementFrom; virtual;
    procedure ForStatementIn; virtual;
    procedure ForStatementTo; virtual;
    procedure ForwardDeclaration; virtual;
    procedure FunctionHeading; virtual;
    procedure FunctionMethodDeclaration; virtual;
    procedure FunctionMethodName; virtual;
    procedure FunctionProcedureBlock; virtual;
    procedure FunctionProcedureName; virtual;
    procedure GotoStatement; virtual;
    procedure Identifier; virtual;
    procedure IdentifierList; virtual;
    procedure IfStatement; virtual;
    procedure TernaryOp; virtual;
    procedure ImplementationSection; virtual;
    procedure ImplementsSpecifier; virtual;
    procedure IncludeFile; virtual;
    procedure IndexSpecifier; virtual;
    procedure IndexOp; virtual;
    procedure InheritedStatement; virtual;
    procedure InheritedVariableReference; virtual;
    procedure InitializationSection; virtual;
    procedure InlineConstSection; virtual;
    procedure InlineStatement; virtual;
    procedure InlineVarDeclaration; virtual;
    procedure InlineVarSection; virtual;
    procedure InParameter; virtual;
    procedure InterfaceDeclaration; virtual;
    procedure InterfaceForward; virtual;
    procedure InterfaceGUID; virtual;
    procedure InterfaceHeritage; virtual;
    procedure InterfaceMemberList; virtual;
    procedure InterfaceSection; virtual;
    procedure InterfaceType; virtual;
    procedure IsNotOp; virtual;
    procedure LabelDeclarationSection; virtual;
    procedure LabeledStatement; virtual;
    procedure LabelId; virtual;
    procedure LibraryFile; virtual;
    procedure LibraryBlock; virtual;
    procedure MainUsedUnitExpression; virtual;
    procedure MainUsedUnitName; virtual;
    procedure MainUsedUnitStatement; virtual;
    procedure MainUsesClause; virtual;
    procedure MethodKind; virtual;
    procedure MultiplicativeOperator; virtual;
    procedure FormalParameterType; virtual;
    procedure NotInOp; virtual;
    procedure NotOp; virtual;
    procedure NilToken; virtual;
    procedure Number; virtual;
    procedure ObjectConstructorHeading; virtual;
    procedure ObjectDestructorHeading; virtual;
    procedure ObjectField; virtual;
    procedure ObjectForward; virtual;
    procedure ObjectFunctionHeading; virtual;
    procedure ObjectHeritage; virtual;
    procedure ObjectMemberList; virtual;
    procedure ObjectMethodDirective; virtual;
    procedure ObjectMethodHeading; virtual;
    procedure ObjectNameOfMethod; virtual;
    procedure ObjectProperty; virtual;
    procedure ObjectPropertySpecifiers; virtual;
    procedure ObjectProcedureHeading; virtual;
    procedure ObjectType; virtual;
    procedure ObjectTypeEnd; virtual;
    procedure ObjectVisibility; virtual;
    procedure OrdinalIdentifier; virtual;
    procedure OrdinalType; virtual;
    procedure OutParameter; virtual;
    procedure PackageFile; virtual;
    procedure ParameterFormal; virtual;
    procedure ParameterName; virtual;
    procedure ParameterNameList; virtual;
    procedure ParseFile; virtual;
    procedure PointerSymbol; virtual;
    procedure PointerType; virtual;
    procedure ProceduralDirective; virtual;
    procedure ProceduralDirectiveOf; virtual;
    procedure ProceduralType; virtual;
    procedure ProcedureDeclarationSection; virtual;
    procedure ProcedureHeading; virtual;
    procedure ProcedureProcedureName; virtual;
    procedure ProcedureMethodName; virtual;
    procedure ProgramBlock; virtual;
    procedure ProgramFile; virtual;
    procedure PropertyDefault; virtual;
    procedure PropertyInterface; virtual;
    procedure PropertyName; virtual;
    procedure PropertyParameterList; virtual;
    procedure PropertySpecifiers; virtual;
    procedure QualifiedIdentifier; virtual;
    procedure RaiseStatement; virtual;
    procedure ReadAccessIdentifier; virtual;
    procedure RealIdentifier; virtual;
    procedure RealType; virtual;
    procedure RecordAlign; virtual;
    procedure RecordAlignValue; virtual;
    procedure RecordConstant; virtual;
    procedure RecordConstraint; virtual;
    procedure RecordFieldConstant; virtual;
    procedure RecordType; virtual;
    procedure RecordVariant; virtual;
    procedure RelativeOperator; virtual;
    procedure RepeatStatement; virtual;
    procedure RequiresClause; virtual;
    procedure RequiresIdentifier; virtual;
    procedure RequiresIdentifierId; virtual;
    procedure ResolutionInterfaceName; virtual;
    procedure ResourceDeclaration; virtual;
    procedure ResourceValue; virtual;
    procedure ReturnType; virtual;
    procedure RoundClose; virtual;
    procedure RoundOpen; virtual;
    procedure SetConstructor; virtual;
    procedure SetElement; virtual;
    procedure SetType; virtual;
    procedure SimpleExpression; virtual;
    procedure SimpleStatement; virtual;
    procedure SimpleType; virtual;
    procedure SkipAnsiComment; virtual;
    procedure SkipBorComment; virtual;
    procedure SkipSlashesComment; virtual;
    procedure SkipSpace; virtual;
    procedure SkipCRLFco; virtual;
    procedure SkipCRLF; virtual;
    procedure Statement; virtual;
    procedure StatementOrExpression; virtual;
    procedure Statements; virtual;
    procedure StatementList; virtual;
    procedure StorageExpression; virtual;
    procedure StorageIdentifier; virtual;
    procedure StorageDefault; virtual;
    procedure StorageNoDefault; virtual;
    procedure StorageSpecifier; virtual;
    procedure StorageStored; virtual;
    procedure StringConst; virtual;
    procedure StringConstSimple; virtual;
    procedure StringIdentifier; virtual;
    procedure StringStatement; virtual;
    procedure StringType; virtual;
    procedure StructuredType; virtual;
    procedure SubrangeType; virtual;
    procedure TagField; virtual;
    procedure TagFieldName; virtual;
    procedure TagFieldTypeName; virtual;
    procedure Term; virtual;
    procedure ThenStatement; virtual;
    procedure ThenExpression; virtual;
    procedure TryStatement; virtual;
    procedure TypedConstant; virtual;
    procedure TypeDeclaration; virtual;
    procedure TypeId; virtual;
    procedure TypeKind; virtual;
    procedure TypeName; virtual;
    procedure TypeReferenceType; virtual;
    procedure TypeSimple; virtual;
    //generics
    procedure TypeArgs; virtual;
    procedure TypeDirective; virtual;
    procedure TypeParams; virtual;
    procedure TypeParamDecl; virtual;
    procedure TypeParamDeclList; virtual;
    procedure TypeParamList; virtual;
    procedure ConstraintList; virtual;
    procedure Constraint; virtual;
    //end generics
    procedure TypeSection; virtual;
    procedure UnaryMinus; virtual;
    procedure UnitFile; virtual;
    procedure UnitId; virtual;
    procedure UnitName; virtual;
    procedure UsedUnitName; virtual;
    procedure UsedUnitsList; virtual;
    procedure UsesClause; virtual;
    procedure VarAbsolute; virtual;
    procedure VarEqual; virtual;
    procedure VarDeclaration; virtual;
    procedure Variable; virtual;
    procedure VariableReference; virtual;
    procedure VariantIdentifier; virtual;
    procedure VariantSection; virtual;
    procedure VarParameter; virtual;
    procedure VarName; virtual;
    procedure VarNameList; virtual;
    procedure VarSection; virtual;
    procedure VisibilityAutomated; virtual;
    procedure VisibilityPrivate; virtual;
    procedure VisibilityProtected; virtual;
    procedure VisibilityPublic; virtual;
    procedure VisibilityPublished; virtual;
    procedure VisibilityStrictPrivate; virtual;
    procedure VisibilityStrictProtected; virtual;
    procedure VisibilityUnknown; virtual;
    procedure WhileStatement; virtual;
    procedure WithExpressionList; virtual;
    procedure WithStatement; virtual;
    procedure WriteAccessIdentifier; virtual;
    //JThurman 2004-03-21
    {This is the syntax for custom attributes, based quite strictly on the
    ECMA syntax specifications for C#, but with a Delphi expression being
    used at the bottom as opposed to a C# expression}
    procedure GlobalAttributes;
    procedure GlobalAttributeSections;
    procedure GlobalAttributeSection;
    procedure GlobalAttributeTargetSpecifier;
    procedure GlobalAttributeTarget;
    procedure Attributes;
    procedure AttributeSections; virtual;
    procedure AttributeSection;
    procedure AttributeTargetSpecifier;
    procedure AttributeTarget;
    procedure AttributeList;
    procedure Attribute; virtual;
    procedure AttributeName; virtual;
    procedure AttributeArguments; virtual;
    procedure PositionalArgumentList;
    procedure PositionalArgument; virtual;
    procedure NamedArgumentList;
    procedure NamedArgument; virtual;
    procedure AttributeArgumentName; virtual;
    procedure AttributeArgumentExpression; virtual;

    property ExID: TptTokenKind read GetExID;
    property GenID: TptTokenKind read GetGenID;
    property TokenID: TptTokenKind read GetTokenID;
    property InRound: Boolean read GetInRound;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SynError(Error: TmwParseError); virtual;
    procedure Run(const UnitName: string; SourceStream: TStream); virtual;

    procedure ClearDefines;
    procedure InitDefinesDefinedByCompiler;
    procedure AddDefine(const ADefine: string);
    procedure RemoveDefine(const ADefine: string);
    function IsDefined(const ADefine: string): Boolean;

    property InterfaceOnly: Boolean read FInterfaceOnly write FInterfaceOnly;
    property Lexer: TmwPasLex read FLexer;
    property OnComment: TCommentEvent read GetOnComment write SetOnComment;
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
    property LastNoJunkPos: Integer read FLastNoJunkPos;
    property LastNoJunkLen: Integer read FLastNoJunkLen;

    property UseDefines: Boolean read GetUseDefines write SetUseDefines;
    property ScopedEnums: Boolean read GetScopedEnums;
    property IncludeHandler: IIncludeHandler write SetIncludeHandler;
  end;

implementation

{ ESyntaxError }

constructor ESyntaxError.Create(const Msg: string);
begin
  FPosXY.X := -1;
  FPosXY.Y := -1;
  inherited Create(Msg);
end;

constructor ESyntaxError.CreateFmt(const Msg: string; const Args: array of const);
begin
  FPosXY.X := -1;
  FPosXY.Y := -1;
  inherited CreateFmt(Msg, Args);
end;

constructor ESyntaxError.CreatePos(const Msg: string; aPosXY: TTokenPoint);
begin
  FPosXY := aPosXY;
  inherited Create(Msg);
end;

{ TmwSimplePasPar }

procedure TmwSimplePasPar.ForwardDeclaration;
begin
  NextToken;
  Semicolon;
end;

procedure TmwSimplePasPar.ObjectProperty;
begin
  Expected(ptProperty);
  PropertyName;
  case TokenID of
    ptColon, ptSquareOpen:
      begin
        PropertyInterface;
      end;
  end;
  ObjectPropertySpecifiers;
  case ExID of
    ptDefault:
      begin
        PropertyDefault;
        Semicolon;
      end;
  end;
end;

procedure TmwSimplePasPar.ObjectPropertySpecifiers;
begin
  if ExID = ptIndex then
  begin
    IndexSpecifier;
  end;
  while ExID in [ptRead, ptReadOnly, ptWrite, ptWriteOnly] do
  begin
    AccessSpecifier;
  end;
  while ExID in [ptDefault, ptNoDefault, ptStored] do
  begin
    StorageSpecifier;
  end;
  Semicolon;
end;

type
  TStringStreamHelper = class helper for TStringStream
    function GetDataString: string;
  {$IFNDEF FPC}
    property DataString: string read GetDataString;
  {$ENDIF}
  end;

function TStringStreamHelper.GetDataString: string;
{$IFNDEF FPC}
var
  Encoding: TEncoding;
begin
  // try to read a bom from the buffer to create the correct encoding
  // but only if the encoding is still the default encoding
  if Self.Encoding = TEncoding.Default then
  begin
    Encoding := nil;
    TEncoding.GetBufferEncoding(Bytes, Encoding);
    Result := Encoding.GetString(Bytes, Length(Encoding.GetPreamble), Size);
  end
  else
    Result := Self.Encoding.GetString(Bytes, 0, Size);
{$ELSE}
var
  Encoding: TEncoding;
  Bytes: TBytes;
begin
  Encoding := nil;
  SetLength(Bytes, Self.Size);
  Bytes := BytesOf(DataString);
  TEncoding.GetBufferEncoding(Bytes, Encoding);
  Result := Encoding.GetString(Bytes, Length(Encoding.GetPreamble), Size);
{$ENDIF}
end;

procedure TmwSimplePasPar.Run(const UnitName: string; SourceStream: TStream);
var
  StringStream: TStringStream;
  OwnStream: Boolean;
{$IFDEF FPC}
  Strings: TStringList;
{$ENDIF}
begin
  OwnStream := not (SourceStream is TStringStream);
  if OwnStream then
  begin
  {$IFNDEF FPC}
    StringStream := TStringStream.Create;
    StringStream.LoadFromStream(SourceStream);
  {$ELSE}
    Strings := TStringList.Create;
    try
      Strings.LoadFromStream(SourceStream);
      StringStream := TStringStream.Create('');
      Strings.SaveToStream(StringStream);
    finally
      FreeAndNil(Strings);
    end;
  {$ENDIF}
  end
  else
    StringStream := TStringStream(SourceStream);
  FLexer.Origin := StringStream.GetDataString;
  ParseFile;
  if OwnStream then
    StringStream.Free;
end;

constructor TmwSimplePasPar.Create;
begin
  inherited Create;
  FLexer := TmwPasLex.Create;
  FLexer.OnCompDirect := HandlePtCompDirect;
  FLexer.OnDefineDirect := HandlePtDefineDirect;
  FLexer.OnElseDirect := HandlePtElseDirect;
  FLexer.OnEndIfDirect := HandlePtEndIfDirect;
  FLexer.OnIfDefDirect := HandlePtIfDefDirect;
  FLexer.OnIfNDefDirect := HandlePtIfNDefDirect;
  FLexer.OnIfOptDirect := HandlePtIfOptDirect;
  FLexer.OnResourceDirect := HandlePtResourceDirect;
  FLexer.OnUnDefDirect := HandlePtUndefDirect;
  FLexer.OnIfDirect := HandlePtIfDirect;
  FLexer.OnIfEndDirect := HandlePtIfEndDirect;
  FLexer.OnElseIfDirect := HandlePtElseIfDirect;
end;

destructor TmwSimplePasPar.Destroy;
begin
  AheadParse.Free;

  FLexer.Free;
  inherited Destroy;
end;

{next two check for ptNull and ExpectedFatal for an EOF Error}

procedure TmwSimplePasPar.Expected(Sym: TptTokenKind);
begin
  if Sym <> Lexer.TokenID then
  begin
    if TokenID = ptNull then
      ExpectedFatal(Sym)
    else
    begin
      if Assigned(FOnMessage) then
        FOnMessage(Self, meError, Format(rsExpected, [TokenName(Sym), FLexer.Token]),
          FLexer.PosXY.X, FLexer.PosXY.Y);
    end;
  end
  else
    NextToken;
end;

procedure TmwSimplePasPar.ExpectedEx(Sym: TptTokenKind);
begin
  if Sym <> Lexer.ExID then
  begin
    if Lexer.TokenID = ptNull then
      ExpectedFatal(Sym) {jdj 7/22/1999}
    else if Assigned(FOnMessage) then
      FOnMessage(Self, meError, Format(rsExpected, ['EX:' + TokenName(Sym), FLexer.Token]),
        FLexer.PosXY.X, FLexer.PosXY.Y);
  end
  else
    NextToken;
end;

{Replace Token with cnEndOfFile if TokenId = ptnull}

procedure TmwSimplePasPar.ExpectedFatal(Sym: TptTokenKind);
var
  tS: string;
begin
  if Sym <> Lexer.TokenID then
  begin
    {--jdj 7/22/1999--}
    if Lexer.TokenId = ptNull then
      tS := rsEndOfFile
    else
      tS := FLexer.Token;
    {--jdj 7/22/1999--}
    raise ESyntaxError.CreatePos(Format(rsExpected, [TokenName(Sym), tS]), FLexer.PosXY);
  end
  else
    NextToken;
end;

procedure TmwSimplePasPar.HandlePtCompDirect(Sender: TmwBasePasLex);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, meNotSupported, 'Currently not supported ' + FLexer.Token, FLexer.PosXY.X, FLexer.PosXY.Y);
  Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtDefineDirect(Sender: TmwBasePasLex);
begin
  Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtElseDirect(Sender: TmwBasePasLex);
begin
  if Sender = Lexer then
    NextToken
  else
    Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtElseIfDirect(Sender: TmwBasePasLex);
begin
  if Sender = Lexer then
    NextToken
  else
    Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtEndIfDirect(Sender: TmwBasePasLex);
begin
  if Sender = Lexer then
    NextToken
  else
    Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtIfDefDirect(Sender: TmwBasePasLex);
begin
  if Sender = Lexer then
    NextToken
  else
    Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtIfDirect(Sender: TmwBasePasLex);
begin
  if Sender = Lexer then
    NextToken
  else
    Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtIfEndDirect(Sender: TmwBasePasLex);
begin
  if Sender = Lexer then
    NextToken
  else
    Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtIfNDefDirect(Sender: TmwBasePasLex);
begin
  if Sender = Lexer then
    NextToken
  else
    Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtIfOptDirect(Sender: TmwBasePasLex);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, meNotSupported, 'Currently not supported ' + FLexer.Token, FLexer.PosXY.X, FLexer.PosXY.Y);
  Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtResourceDirect(Sender: TmwBasePasLex);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, meNotSupported, 'Currently not supported ' + FLexer.Token, FLexer.PosXY.X, FLexer.PosXY.Y);
  Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtUndefDirect(Sender: TmwBasePasLex);
begin
  Sender.Next;
end;

procedure TmwSimplePasPar.NextToken;
begin
  FLexer.NextNoJunk;
end;

procedure TmwSimplePasPar.NilToken;
begin
  Expected(ptNil);
end;

procedure TmwSimplePasPar.NotInOp;
begin
  Expected(ptNot);
  Expected(ptIn);
end;

procedure TmwSimplePasPar.NotOp;
begin
  Expected(ptNot);
end;

procedure TmwSimplePasPar.SkipJunk;
begin
  if Lexer.IsJunk then
  begin
    case TokenID of
      ptAnsiComment:
        begin
          SkipAnsiComment;
        end;
      ptBorComment:
        begin
          SkipBorComment;
        end;
      ptSlashesComment:
        begin
          SkipSlashesComment;
        end;
      ptSpace:
        begin
          SkipSpace;
        end;
      ptCRLFCo:
        begin
          SkipCRLFco;
        end;
      ptCRLF:
        begin
          SkipCRLF;
        end;
      ptSquareOpen:
        begin
          CustomAttribute;
        end;
    else
      begin
        Lexer.Next;
      end;
    end;
  end;
  FLastNoJunkPos := Lexer.TokenPos;
  FLastNoJunkLen := Lexer.TokenLen;
end;

procedure TmwSimplePasPar.SkipAnsiComment;
begin
  Expected(ptAnsiComment);
  while TokenID in [ptAnsiComment] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.SkipBorComment;
begin
  Expected(ptBorComment);
  while TokenID in [ptBorComment] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.SkipSlashesComment;
begin
  Expected(ptSlashesComment);
end;

procedure TmwSimplePasPar.ThenExpression;
begin
  Expected(ptThen);
  Expression;
end;

procedure TmwSimplePasPar.ThenStatement;
begin
  Expected(ptThen);
  Statement;
end;

procedure TmwSimplePasPar.Semicolon;
begin
  case Lexer.TokenID of
    ptElse, ptEnd, ptExcept, ptfinally, ptFinalization, ptRoundClose, ptUntil: ;
  else
    Expected(ptSemiColon);
  end;
end;

function TmwSimplePasPar.GetExID: TptTokenKind;
begin
  Result := FLexer.ExID;
end;

function TmwSimplePasPar.GetTokenID: TptTokenKind;
begin
  Result := FLexer.TokenID;
end;

function TmwSimplePasPar.GetUseDefines: Boolean;
begin
  Result := FLexer.UseDefines;
end;

function TmwSimplePasPar.GetScopedEnums: Boolean;
begin
  Result := FLexer.ScopedEnums;
end;

procedure TmwSimplePasPar.GotoStatement;
begin
  Expected(ptGoto);
  LabelId;
end;

function TmwSimplePasPar.GetGenID: TptTokenKind;
begin
  Result := FLexer.GenID;
end;

function TmwSimplePasPar.GetInRound: Boolean;
begin
  Result := FInRound > 0;
end;

function TmwSimplePasPar.GetOnComment: TCommentEvent;
begin
  Result := FLexer.OnComment;
end;

procedure TmwSimplePasPar.SynError(Error: TmwParseError);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, meError, ParserErrorName(Error) + ' found ' + FLexer.Token, FLexer.PosXY.X,
      FLexer.PosXY.Y);

end;

(******************************************************************************
 This part is oriented at the official grammar of Delphi 4
 and parialy based on Robert Zierers Delphi grammar.
 For more information about Delphi grammars take a look at:
 http://www.stud.mw.tu-muenchen.de/~rz1/Grammar.html
******************************************************************************)

procedure TmwSimplePasPar.ParseFile;
begin
  SkipJunk;
  case GenID of
    ptLibrary:
      begin
        LibraryFile;
      end;
    ptPackage:
      begin
        PackageFile;
      end;
    ptProgram:
      begin
        ProgramFile;
      end;
    ptUnit:
      begin
        UnitFile;
      end;
  else
    begin
      IncludeFile;
    end;
  end;
end;

procedure TmwSimplePasPar.LibraryFile;
begin
  Expected(ptLibrary);
  UnitName;
  Semicolon;

  LibraryBlock;
  Expected(ptPoint);
end;

procedure TmwSimplePasPar.LibraryBlock;
begin
  if TokenID = ptUses then
    MainUsesClause;

  DeclarationSections;

  if TokenID = ptBegin then
    CompoundStatement
  else
    Expected(ptEnd);
end;

procedure TmwSimplePasPar.PackageFile;
begin
  ExpectedEx(ptPackage);
  UnitName;
  Semicolon;
  case ExID of
    ptRequires:
      begin
        RequiresClause;
      end;
  end;
  case ExID of
    ptContains:
      begin
        ContainsClause;
      end;
  end;

  while Lexer.TokenID = ptSquareOpen do
  begin
    CustomAttribute;
  end;

  Expected(ptEnd);
  Expected(ptPoint);
end;

procedure TmwSimplePasPar.ProgramFile;
begin
  Expected(ptProgram);
  UnitName;
  if TokenID = ptRoundOpen then
  begin
    NextToken;
    IdentifierList;
    Expected(ptRoundClose);
  end;
  if not InterfaceOnly then
  begin
    Semicolon;
    ProgramBlock;
    Expected(ptPoint);
  end;
end;

procedure TmwSimplePasPar.UnaryMinus;
begin
  Expected(ptMinus);
end;

procedure TmwSimplePasPar.UnitFile;
begin
  Expected(ptUnit);
  UnitName;
  TypeDirective;

  Semicolon;
  InterfaceSection;
  if not InterfaceOnly then
  begin
    ImplementationSection;
    case TokenID of
      ptInitialization:
        begin
          InitializationSection;
          if TokenID = ptFinalization then
            FinalizationSection;
          Expected(ptEnd);
        end;
      ptBegin:
        begin
          CompoundStatement;
        end;
      ptEnd:
        begin
          NextToken;
        end;
    end;

    Expected(ptPoint);
  end;
end;

procedure TmwSimplePasPar.ProgramBlock;
begin
  if TokenID = ptUses then
  begin
    MainUsesClause;
  end;
  Block;
end;

procedure TmwSimplePasPar.MainUsesClause;
begin
  Expected(ptUses);
  MainUsedUnitStatement;
  while TokenID = ptComma do
  begin
    NextToken;
    MainUsedUnitStatement;
  end;
  Semicolon;
end;

procedure TmwSimplePasPar.MethodKind;
begin
  case TokenID of
    ptConstructor:
      begin
        NextToken;
      end;
    ptDestructor:
      begin
        NextToken;
      end;
    ptProcedure:
      begin
        NextToken;
      end;
    ptFunction:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidProcedureMethodDeclaration);
    end;
  end;
end;

procedure TmwSimplePasPar.MainUsedUnitStatement;
begin
  MainUsedUnitName;
  if Lexer.TokenID = ptIn then
  begin
    NextToken;
    MainUsedUnitExpression;
  end;
end;

procedure TmwSimplePasPar.MainUsedUnitName;
begin
  UsedUnitName;
end;

procedure TmwSimplePasPar.MainUsedUnitExpression;
begin
  ConstantExpression;
end;

procedure TmwSimplePasPar.UsesClause;
begin
  Expected(ptUses);
  UsedUnitsList;
  Semicolon;
end;

procedure TmwSimplePasPar.UsedUnitsList;
begin
  UsedUnitName;
  while TokenID = ptComma do
  begin
    NextToken;
    UsedUnitName;
  end;
end;

procedure TmwSimplePasPar.UsedUnitName;
begin
  UnitId;
  while Lexer.TokenID = ptPoint do
  begin
    NextToken;
    UnitId;
  end;
end;

procedure TmwSimplePasPar.Block;
begin
  DeclarationSections;
  case TokenID of
    ptAsm:
      begin
        AsmStatement;
      end;
  else
    begin
      CompoundStatement;
    end;
  end;
end;

procedure TmwSimplePasPar.DeclarationSection;
begin
  case TokenID of
    ptClass:
      begin
        ProcedureDeclarationSection;
      end;
    ptConst:
      begin
        ConstSection;
      end;
    ptConstructor:
      begin
        ProcedureDeclarationSection;
      end;
    ptDestructor:
      begin
        ProcedureDeclarationSection;
      end;
    ptExports:
      begin
        ExportsClause;
      end;
    ptFunction:
      begin
        ProcedureDeclarationSection;
      end;
    ptLabel:
      begin
        LabelDeclarationSection;
      end;
    ptProcedure:
      begin
        ProcedureDeclarationSection;
      end;
    ptResourceString:
      begin
        ConstSection;
      end;
    ptType:
      begin
        TypeSection;
      end;
    ptThreadVar:
      begin
        VarSection;
      end;
    ptVar:
      begin
        VarSection;
      end;
    ptSquareOpen:
      begin
        CustomAttribute;
      end;
  else
    begin
      SynError(InvalidDeclarationSection);
    end;
  end;
end;

procedure TmwSimplePasPar.UnitId;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.UnitName;
begin
  UnitId;
  while Lexer.TokenID = ptPoint do
  begin
    NextToken;
    UnitId;
  end;
end;

procedure TmwSimplePasPar.InterfaceHeritage;
begin
  Expected(ptRoundOpen);
  AncestorIdList;
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.InterfaceGUID;
begin
  Expected(ptSquareOpen);
  CharString;
  Expected(ptSquareClose);
end;

procedure TmwSimplePasPar.AccessSpecifier;
begin
  case ExID of
    ptRead:
      begin
        NextToken;
        ReadAccessIdentifier;
      end;
    ptWrite:
      begin
        NextToken;
        WriteAccessIdentifier;
      end;
    ptReadOnly:
      begin
        NextToken;
      end;
    ptWriteOnly:
      begin
        NextToken;
      end;
    ptAdd:
      begin
        NextToken;
        QualifiedIdentifier; //TODO: AddAccessIdentifier
      end;
    ptRemove:
      begin
        NextToken;
        QualifiedIdentifier; //TODO: RemoveAccessIdentifier
      end;
  else
    begin
      SynError(InvalidAccessSpecifier);
    end;
  end;
end;

procedure TmwSimplePasPar.ReadAccessIdentifier;
begin
  variable;
end;

procedure TmwSimplePasPar.WriteAccessIdentifier;
begin
  variable;
end;

procedure TmwSimplePasPar.StorageSpecifier;
begin
  case ExID of
    ptStored:
      begin
        StorageStored;
      end;
    ptDefault:
      begin
        StorageDefault;
      end;
    ptNoDefault:
      begin
        StorageNoDefault;
      end
  else
    begin
      SynError(InvalidStorageSpecifier);
    end;
  end;
end;

procedure TmwSimplePasPar.StorageDefault;
begin
  ExpectedEx(ptDefault);
  StorageExpression;
end;

procedure TmwSimplePasPar.StorageNoDefault;
begin
  ExpectedEx(ptNoDefault);
end;

procedure TmwSimplePasPar.StorageStored;
begin
  ExpectedEx(ptStored);
  case TokenID of
    ptIdentifier:
      begin
        StorageIdentifier;
      end;
  else
    if TokenID <> ptSemiColon then
    begin
      StorageExpression;
    end;
  end;
end;

procedure TmwSimplePasPar.StorageExpression;
begin
  ConstantExpression;
end;

procedure TmwSimplePasPar.StorageIdentifier;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.PropertyParameterList;
begin
  Expected(ptSquareOpen);
  FormalParameterSection;
  while TokenID = ptSemiColon do
  begin
    Semicolon;
    FormalParameterSection;
  end;
  Expected(ptSquareClose);
end;

procedure TmwSimplePasPar.PropertySpecifiers;
begin
  if ExID = ptIndex then
  begin
    IndexSpecifier;
  end;
  while ExID in [ptRead, ptReadOnly, ptWrite, ptWriteOnly, ptAdd, ptRemove] do
  begin
    AccessSpecifier;
    if TokenID = ptSemicolon then
      NextToken;
  end;
  if ExID = ptDispId then
  begin
    DispIDSpecifier;
  end;
  while ExID in [ptDefault, ptNoDefault, ptStored] do
  begin
    StorageSpecifier;
    if TokenID = ptSemicolon then
      NextToken;
  end;
  if ExID = ptImplements then
  begin
    ImplementsSpecifier;
  end;
  if TokenID = ptSemicolon then
    NextToken;
end;

procedure TmwSimplePasPar.PropertyInterface;
begin
  if TokenID = ptSquareOpen then
  begin
    PropertyParameterList;
  end;
  Expected(ptColon);
  TypeID;
end;

procedure TmwSimplePasPar.ClassMethodHeading;
begin
  if TokenID = ptClass then
    ClassClass;

  InitAhead;
  AheadParse.NextToken;
  AheadParse.FunctionProcedureName;

  if AheadParse.TokenId = ptEqual then
    ClassMethodResolution
  else
  begin
    case TokenID of
      ptConstructor:
        begin
          ConstructorHeading;
        end;
      ptDestructor:
        begin
          DestructorHeading;
        end;
      ptFunction:
        begin
          ClassFunctionHeading;
        end;
      ptProcedure:
        begin
          ClassProcedureHeading;
        end;
      ptIdentifier:
        begin
          if Lexer.ExID = ptOperator then
          begin
            ClassOperatorHeading;
          end
          else
            SynError(InvalidProcedureMethodDeclaration);
        end;
    else
      SynError(InvalidClassMethodHeading);
    end;
  end;
end;

procedure TmwSimplePasPar.ClassFunctionHeading;
begin
  Expected(ptFunction);
  FunctionProcedureName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  Expected(ptColon);
  ReturnType;
  if TokenId = ptSemicolon then
    Semicolon;
  if ExID in ClassMethodDirectiveEnum then
    ClassMethodDirective;
end;

procedure TmwSimplePasPar.FunctionMethodName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.ClassProcedureHeading;
begin
  Expected(ptProcedure);
  FunctionProcedureName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  if TokenId = ptSemicolon then
    Semicolon;

  if ExID = ptDispId then
  begin
    DispIDSpecifier;
    if TokenId = ptSemicolon then
      Semicolon;
  end;
  if exID in ClassMethodDirectiveEnum then
  ClassMethodDirective;
end;

procedure TmwSimplePasPar.ProcedureMethodName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.ClassMethodResolution;
begin
  case TokenID of
    ptFunction:
      begin
        NextToken;
      end;
    ptProcedure:
      begin
        NextToken;
      end;
    ptIdentifier:
      begin
        if Lexer.ExID = ptOperator then
          NextToken;
      end;
  end;
  FunctionProcedureName;
  Expected(ptEqual);
  FunctionMethodName;
  Semicolon;
end;

procedure TmwSimplePasPar.ClassOperatorHeading;
begin
  ExpectedEx(ptOperator);
  FunctionProcedureName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;

  if TokenID = ptColon then
  begin
    Expected(ptColon);
    ReturnType;
  end;

  if TokenId = ptSemicolon then
    Semicolon;
  if ExID in ClassMethodDirectiveEnum then
    ClassMethodDirective;
end;

procedure TmwSimplePasPar.ResolutionInterfaceName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.Constraint;
begin
  while TokenId in [ptConstructor, ptRecord, ptClass, ptIdentifier] do
  begin
    case TokenId of
      ptConstructor: ConstructorConstraint;
      ptRecord: RecordConstraint;
      ptClass: ClassConstraint;
      ptIdentifier: TypeId;
    end;
    if TokenId = ptComma then
      NextToken;
  end;
end;

procedure TmwSimplePasPar.ConstraintList;
begin
  Constraint;
  while TokenId = ptComma do
  begin
    Constraint;
  end;
end;

procedure TmwSimplePasPar.ConstructorConstraint;
begin
  Expected(ptConstructor);
end;

procedure TmwSimplePasPar.ConstructorHeading;
begin
  Expected(ptConstructor);
  ConstructorName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  if TokenID = ptSemiColon then Semicolon;
  ClassMethodDirective;
end;

procedure TmwSimplePasPar.ConstructorName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.DestructorHeading;
begin
  Expected(ptDestructor);
  DestructorName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  if TokenID = ptSemiColon then Semicolon;
  ClassMethodDirective;
end;

procedure TmwSimplePasPar.DestructorName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.ClassMethod;
begin
  Expected(ptClass);
end;

procedure TmwSimplePasPar.ClassMethodDirective;
begin
  while ExId in ClassMethodDirectiveEnum do
  begin
    if ExID = ptDispId then
      DispIDSpecifier
    else
      ProceduralDirective;
    if TokenId = ptSemicolon then
      Semicolon;
  end;
end;

procedure TmwSimplePasPar.ObjectMethodHeading;
begin
  case TokenID of
    ptConstructor:
      begin
        ObjectConstructorHeading;
      end;
    ptDestructor:
      begin
        ObjectDestructorHeading;
      end;
    ptFunction:
      begin
        ObjectFunctionHeading;
      end;
    ptProcedure:
      begin
        ObjectProcedureHeading;
      end;
  else
    begin
      SynError(InvalidMethodHeading);
    end;
  end;
end;

procedure TmwSimplePasPar.ObjectFunctionHeading;
begin
  Expected(ptFunction);
  FunctionMethodName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  Expected(ptColon);
  ReturnType;
  if TokenID = ptSemiColon then  Semicolon;
  ObjectMethodDirective;
end;

procedure TmwSimplePasPar.ObjectProcedureHeading;
begin
  Expected(ptProcedure);
  ProcedureMethodName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  if TokenID = ptSemiColon then Semicolon;
  ObjectMethodDirective;
end;

procedure TmwSimplePasPar.ObjectConstructorHeading;
begin
  Expected(ptConstructor);
  ConstructorName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  if TokenID = ptSemiColon then Semicolon;
  ObjectMethodDirective;
end;

procedure TmwSimplePasPar.ObjectDestructorHeading;
begin
  Expected(ptDestructor);
  DestructorName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  if TokenID = ptSemiColon then Semicolon;
  ObjectMethodDirective;
end;

procedure TmwSimplePasPar.ObjectMethodDirective;
begin
  while ExID in [ptAbstract, ptCdecl, ptDynamic, ptExport, ptExternal, ptFar,
    ptMessage, ptNear, ptOverload, ptPascal, ptRegister, ptSafeCall, ptStdCall,
    ptVirtual, ptDeprecated, ptLibrary, ptPlatform, ptStatic, ptInline] do
  begin
    ProceduralDirective;
    if TokenID = ptSemiColon then Semicolon;
  end;
end;

procedure TmwSimplePasPar.Directive16Bit;
begin
  case ExID of
    ptNear:
      begin
        NextToken;
      end;
    ptFar:
      begin
        NextToken;
      end;
    ptExport:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidDirective16Bit);
    end;
  end;
end;

procedure TmwSimplePasPar.DirectiveBinding;
begin
  case ExID of
    ptAbstract:
      begin
        NextToken;
      end;
    ptVirtual:
      begin
        NextToken;
      end;
    ptDynamic:
      begin
        NextToken;
      end;
    ptMessage:
      begin
        DirectiveBindingMessage;
      end;
    ptOverride:
      begin
        NextToken;
      end;
    ptOverload:
      begin
        NextToken;
      end;
    ptReintroduce:
      begin
        NextToken;
      end;
    ptNoreturn:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidDirectiveBinding);
    end;
  end;
end;

procedure TmwSimplePasPar.DirectiveBindingMessage;
begin
  NextToken;
  ConstantExpression;
end;

procedure TmwSimplePasPar.ReturnType;
begin
  while TokenID = ptSquareOpen do
    CustomAttribute;

  TypeID;
end;

procedure TmwSimplePasPar.RoundClose;
begin
  Expected(ptRoundClose);
  Dec(FInRound);
end;

procedure TmwSimplePasPar.RoundOpen;
begin
  Expected(ptRoundOpen);
  Inc(FInRound);
end;

procedure TmwSimplePasPar.FormalParameterList;
begin
  Expected(ptRoundOpen);
  FormalParameterSection;
  while TokenID = ptSemiColon do
  begin
    Semicolon;
    FormalParameterSection;
  end;
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.FormalParameterSection;
begin
  while TokenID = ptSquareOpen do
    CustomAttribute;
  case TokenID of
    ptConst:
      begin
        ConstParameter;
      end;
    ptIdentifier:
      case ExID of
        ptOut: OutParameter;
      else
        ParameterFormal;
      end;
    ptIn:
      begin
        InParameter;
      end;
    ptVar:
      begin
        VarParameter;
      end;
  end;
end;

procedure TmwSimplePasPar.ConstParameter;
begin
  Expected(ptConst);
  ParameterNameList;
  case TokenID of
    ptColon:
      begin
        NextToken;
        FormalParameterType;
        if TokenID = ptEqual then
        begin
          NextToken;
          TypedConstant;
        end;
      end
  end;
end;

procedure TmwSimplePasPar.VarParameter;
begin
  Expected(ptVar);
  ParameterNameList;
  case TokenID of
    ptColon:
      begin
        NextToken;
        FormalParameterType;
      end
  end;
end;

procedure TmwSimplePasPar.OutParameter;
begin
  ExpectedEx(ptOut);
  ParameterNameList;
  case TokenID of
    ptColon:
      begin
        NextToken;
        FormalParameterType;
      end
  end;
end;

procedure TmwSimplePasPar.ParameterFormal;
begin
  case TokenID of
    ptIdentifier:
      begin
        ParameterNameList;
        Expected(ptColon);
        FormalParameterType;
        if TokenID = ptEqual then
        begin
          NextToken;
          TypedConstant;
        end;
      end;
  else
    begin
      SynError(InvalidParameter);
    end;
  end;
end;

procedure TmwSimplePasPar.ParameterNameList;
begin
  while TokenID = ptSquareOpen do
    CustomAttribute;
  ParameterName;

  while TokenID = ptComma do
  begin
    NextToken;

    while TokenID = ptSquareOpen do
      CustomAttribute;
    ParameterName;
  end;
end;

procedure TmwSimplePasPar.ParameterName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.FormalParameterType;
begin
  if TokenID = ptArray then
    StructuredType
  else
    TypeID;
end;

procedure TmwSimplePasPar.FunctionMethodDeclaration;
begin
  if (TokenID = ptIdentifier) and (Lexer.ExID = ptOperator) then
    NextToken else
  MethodKind;
  FunctionProcedureName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  case TokenID of
    ptSemiColon:
      begin
        FunctionProcedureBlock;
      end;
  else
    begin
      Expected(ptColon);
      ReturnType;
      FunctionProcedureBlock;
    end;
  end;
end;

procedure TmwSimplePasPar.ProcedureProcedureName;
begin
  MethodKind;
  FunctionProcedureName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  FunctionProcedureBlock;
end;

procedure TmwSimplePasPar.FunctionProcedureName;
begin
  ObjectNameOfMethod;
end;

procedure TmwSimplePasPar.ObjectNameOfMethod;
begin
  if TokenID = ptIn then
    Expected(ptIn)
  else
    Expected(ptIdentifier);

  if TokenId = ptLower then
    TypeParams;
  if TokenID = ptPoint then
  begin
    Expected(ptPoint);
    ObjectNameOfMethod;
  end;
end;

procedure TmwSimplePasPar.FunctionProcedureBlock;
var
  HasBlock: Boolean;
begin
  HasBlock := True;
  if TokenID = ptSemiColon then Semicolon;

  while ExID in [ptAbstract, ptCdecl, ptDynamic, ptExport, ptExternal, ptDelayed, ptFar,
    ptMessage, ptNear, ptOverload, ptOverride, ptPascal, ptRegister,
    ptReintroduce, ptSafeCall, ptStdCall, ptVirtual, ptLibrary,
    ptPlatform, ptLocal, ptVarargs, ptAssembler, ptStatic, ptInline, ptForward,
    ptExperimental, ptDeprecated, ptNoreturn] do
  begin
    case ExId of
      ptExternal:
        begin
          ProceduralDirective;
          HasBlock := False;
        end;
      ptForward:
        begin
          ForwardDeclaration;
          HasBlock := False;
        end
    else
      begin
        ProceduralDirective;
      end;
    end;
    if TokenID = ptSemiColon then Semicolon;
  end;

  if HasBlock then
  begin
    case TokenID of
      ptAsm:
        begin
          AsmStatement;
        end;
    else
      begin
        Block;
      end;
    end;
    Semicolon;
  end;
end;

procedure TmwSimplePasPar.ExternalDirective;
begin
  ExpectedEx(ptExternal);
  case TokenID of
    ptSemiColon:
      begin
        Semicolon;
      end;
  else
    begin
      if FLexer.ExID <> ptName then
        SimpleExpression;

      if FLexer.ExID = ptDelayed then
        NextToken;

      ExternalDirectiveTwo;
    end;
  end;
end;

procedure TmwSimplePasPar.ExternalDirectiveTwo;
begin
  case FLexer.ExID of
    ptIndex:
      begin
        IndexSpecifier;
      end;
    ptName:
      begin
        NextToken;
        SimpleExpression;
      end;
    ptSemiColon:
      begin
        Semicolon;
        ExternalDirectiveThree;
      end;
  end
end;

procedure TmwSimplePasPar.ExternalDirectiveThree;
begin
  case TokenID of
    ptMinus:
      begin
        NextToken;
      end;
  end;
  case TokenID of
    ptIdentifier, ptIntegerConst:
      begin
        NextToken;
      end;
  end;
end;

procedure TmwSimplePasPar.ForStatement;
begin
  Expected(ptFor);
  if TokenID = ptVar then
  begin
    NextToken;
    InlineVarDeclaration;
  end
  else
    QualifiedIdentifier;

  if Lexer.TokenID = ptAssign then
  begin
    Expected(ptAssign);
    ForStatementFrom;
    case TokenID of
      ptTo:
        begin
          ForStatementTo;
        end;
      ptDownTo:
        begin
          ForStatementDownTo;
        end;
    else
      begin
        SynError(InvalidForStatement);
      end;
    end;
  end else
  if Lexer.TokenID = ptIn then
    ForStatementIn;
  Expected(ptDo);
  Statement;
end;

procedure TmwSimplePasPar.ForStatementDownTo;
begin
  Expected(ptDownTo);
  Expression;
end;

procedure TmwSimplePasPar.ForStatementFrom;
begin
  Expression;
end;

procedure TmwSimplePasPar.ForStatementIn;
begin
  Expected(ptIn);
  Expression;
end;

procedure TmwSimplePasPar.ForStatementTo;
begin
  Expected(ptTo);
  Expression;
end;

procedure TmwSimplePasPar.WhileStatement;
begin
  Expected(ptWhile);
  Expression;
  Expected(ptDo);
  Statement;
end;

procedure TmwSimplePasPar.RepeatStatement;
begin
  Expected(ptRepeat);
  StatementList;
  Expected(ptUntil);
  Expression;
end;

procedure TmwSimplePasPar.CaseStatement;
begin
  Expected(ptCase);
  Expression;
  Expected(ptOf);
  CaseSelector;
  while TokenID = ptSemiColon do
  begin
    Semicolon;
    case TokenID of
      ptElse, ptEnd: ;
    else
      CaseSelector;
    end;
  end;
  if TokenID = ptElse then
    CaseElseStatement;
  Expected(ptEnd);
end;

procedure TmwSimplePasPar.CaseSelector;
begin
  CaseLabelList;
  Expected(ptColon);
  case TokenID of
    ptSemiColon: EmptyStatement;
  else
    Statement;
  end;
end;

procedure TmwSimplePasPar.CaseElseStatement;
begin
  Expected(ptElse);
  StatementList;
  Semicolon;
end;

procedure TmwSimplePasPar.CaseLabel;
begin
  ConstantExpression;
  if TokenID = ptDotDot then
  begin
    NextToken;
    ConstantExpression;
  end;
end;

procedure TmwSimplePasPar.IfStatement;
begin
  Expected(ptIf);
  Expression;
  ThenStatement;
  if TokenID = ptElse then
    ElseStatement;
end;

procedure TmwSimplePasPar.TernaryOp;
begin
  Expected(ptIf);
  Expression;
  ThenExpression;
  ElseExpression;
end;

procedure TmwSimplePasPar.ExceptBlock;
begin
  if ExID = ptOn then
  begin
    ExceptionHandlerList;
    if TokenID = ptElse then
      ExceptionBlockElseBranch;
  end else
    if TokenID = ptElse then
      ExceptionBlockElseBranch
    else
      StatementList;
end;

procedure TmwSimplePasPar.ExceptionHandlerList;
begin
  while FLexer.ExID = ptOn do
  begin
    ExceptionHandler;
    Semicolon;
  end;
end;

procedure TmwSimplePasPar.ExceptionHandler;
begin
  ExpectedEx(ptOn);
  ExceptionIdentifier;
  Expected(ptDo);
  Statement;
end;

procedure TmwSimplePasPar.ExceptionBlockElseBranch;
begin
  NextToken;
  StatementList;
end;

procedure TmwSimplePasPar.ExceptionIdentifier;
begin
  Lexer.InitAhead;
  case Lexer.AheadTokenID of
    ptPoint:
      begin
        ExceptionClassTypeIdentifier;
      end;
    ptColon:
      begin
        ExceptionVariable;
      end
  else
    begin
      ExceptionClassTypeIdentifier;
    end;
  end;
end;

procedure TmwSimplePasPar.ExceptionClassTypeIdentifier;
begin
  TypeKind;
end;

procedure TmwSimplePasPar.ExceptionVariable;
begin
  Expected(ptIdentifier);
  Expected(ptColon);
  ExceptionClassTypeIdentifier;
end;

procedure TmwSimplePasPar.InlineConstSection;
begin
  case TokenID of
    ptConst:
      begin
        NextToken;
        ConstantDeclaration;
      end;
  else
    begin
      SynError(InvalidConstSection);
    end;
  end;
end;

procedure TmwSimplePasPar.InlineStatement;
begin
  Expected(ptInline);
  Expected(ptRoundOpen);
  Expected(ptIntegerConst);
  while (TokenID = ptSlash) do
  begin
    NextToken;
    Expected(ptIntegerConst);
  end;
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.InlineVarSection;
begin
  Expected(ptVar);
  while TokenID = ptIdentifier do
    InlineVarDeclaration;

  if TokenID = ptAssign then
  begin
    NextToken;
    Expression;
  end;
end;

procedure TmwSimplePasPar.InlineVarDeclaration;
begin
  VarNameList;
  if TokenID = ptColon then
  begin
    NextToken;
    TypeKind;
  end;
end;

procedure TmwSimplePasPar.InParameter;
begin
  Expected(ptIn);
  ParameterNameList;
  case TokenID of
    ptColon:
      begin
        NextToken;
        FormalParameterType;
        if TokenID = ptEqual then
        begin
          NextToken;
          TypedConstant;
        end;
      end
  end;
end;

procedure TmwSimplePasPar.AsmStatement;
begin
  Lexer.AsmCode := True;
  Expected(ptAsm);
  { should be replaced with a Assembler lexer }
  while TokenID <> ptEnd do
    case FLexer.TokenID of
      ptAddressOp:
        begin
          NextToken;
          NextToken;
        end;
      ptDoubleAddressOp:
        begin
          NextToken;
          NextToken;
        end;
      ptNull:
        begin
          Expected(ptEnd);
          Exit;
        end;
    else
      NextToken;
    end;
  Lexer.AsmCode := False;
  Expected(ptEnd);
end;

procedure TmwSimplePasPar.AsOp;
begin
  Expected(ptAs);
end;

procedure TmwSimplePasPar.AssignOp;
begin
  Expected(ptAssign);
end;

procedure TmwSimplePasPar.AtExpression;
begin
  ExpectedEx(ptAt);
  Expression;
end;

procedure TmwSimplePasPar.RaiseStatement;
begin
  Expected(ptRaise);
  case TokenID of
    ptAddressOp, ptDoubleAddressOp, ptIdentifier, ptRoundOpen:
      begin
        Expression;
      end;
  end;
  if ExID = ptAt then
    AtExpression;
end;

procedure TmwSimplePasPar.TryStatement;
begin
  Expected(ptTry);
  StatementList;
  case TokenID of
    ptExcept:
      begin
        NextToken;
        ExceptBlock;
        Expected(ptEnd);
      end;
    ptFinally:
      begin
        NextToken;
        FinallyBlock;
        Expected(ptEnd);
      end;
  else
    begin
      SynError(InvalidTryStatement);
    end;
  end;
end;

procedure TmwSimplePasPar.WithStatement;
begin
  Expected(ptWith);
  WithExpressionList;
  Expected(ptDo);
  Statement;
end;

procedure TmwSimplePasPar.WithExpressionList;
begin
  Expression;
  while FLexer.TokenID = ptComma do
  begin
    NextToken;
    Expression;
  end;
end;

procedure TmwSimplePasPar.StatementList;
begin
  Statements;
end;

procedure TmwSimplePasPar.StatementOrExpression;
begin
  if TokenID = ptGoto then
    SimpleStatement
  else
  begin
    InitAhead;
    AheadParse.Designator;

    if AheadParse.TokenId in [ptAssign, ptSemicolon, ptElse] then
      SimpleStatement
    else
      Expression;
  end;
end;

procedure TmwSimplePasPar.Statements;
begin {removed ptIntegerConst jdj-Put back in for labels}
  while TokenID in [ptAddressOp, ptAsm, ptBegin, ptCase, ptConst, ptDoubleAddressOp,
    ptFor, ptGoTo, ptIdentifier, ptIf, ptInherited, ptInline, ptIntegerConst,
    ptPointerSymbol, ptRaise, ptRoundOpen, ptRepeat, ptSemiColon, ptString,
    ptTry, ptVar, ptWhile, ptWith] do
  begin
    Statement;
    Semicolon;
  end;
end;

procedure TmwSimplePasPar.SimpleStatement;
begin
  case TokenID of
    ptAddressOp, ptDoubleAddressOp, ptIdentifier, ptRoundOpen, ptString:
      begin
        Designator;
        if TokenID = ptAssign then
        begin
          AssignOp;
          Expression;
        end;
      end;
    ptGoTo:
      begin
        GotoStatement;
      end;
  end;
end;

procedure TmwSimplePasPar.Statement;
begin
  case TokenID of
    ptAsm:
      begin
        AsmStatement;
      end;
    ptBegin:
      begin
        CompoundStatement;
      end;
    ptCase:
      begin
        CaseStatement;
      end;
    ptConst:
      begin
        InlineConstSection;
      end;
    ptFor:
      begin
        ForStatement;
      end;
    ptIf:
      begin
        IfStatement;
      end;
    ptIdentifier:
      begin
        FLexer.InitAhead;
        case Lexer.AheadTokenID of
          ptColon:
            begin
              LabeledStatement;
            end;
        else
          begin
            StatementOrExpression;
          end;
        end;
      end;
    ptInherited:
      begin
        InheritedStatement;
      end;
    ptInLine:
      begin
        InlineStatement;
      end;
    ptIntegerConst:
      begin
        FLexer.InitAhead;
        case Lexer.AheadTokenID of
          ptColon:
            begin
              LabeledStatement;
            end;
        else
          begin
            SynError(InvalidLabeledStatement);
            NextToken;
          end;
        end;
      end;
    ptRepeat:
      begin
        RepeatStatement;
      end;
    ptRaise:
      begin
        RaiseStatement;
      end;
    ptSemiColon:
      begin
        EmptyStatement;
      end;
    ptTry:
      begin
        TryStatement;
      end;
    ptVar:
      begin
        InlineVarSection;
      end;
    ptWhile:
      begin
        WhileStatement;
      end;
    ptWith:
      begin
        WithStatement;
      end;
  else
    begin
      StatementOrExpression;
    end;
  end;
end;

procedure TmwSimplePasPar.ElseExpression;
begin
  Expected(ptElse);
  Expression;
end;

procedure TmwSimplePasPar.ElseStatement;
begin
  Expected(ptElse);
  Statement;
end;

procedure TmwSimplePasPar.EmptyStatement;
begin
  { Nothing to do here.
    The semicolon will be removed in StatementList }
end;

procedure TmwSimplePasPar.InheritedStatement;
begin
  Expected(ptInherited);
  if TokenID = ptIdentifier then
    Statement;
end;

procedure TmwSimplePasPar.LabeledStatement;
begin
  case TokenID of
    ptIdentifier:
      begin
        NextToken;
        Expected(ptColon);
        Statement;
      end;
    ptIntegerConst:
      begin
        NextToken;
        Expected(ptColon);
        Statement;
      end;
  else
    begin
      SynError(InvalidLabeledStatement);
    end;
  end;
end;

procedure TmwSimplePasPar.StringStatement;
begin
  Expected(ptString);
end;

procedure TmwSimplePasPar.SetElement;
begin
  Expression;
  if TokenID = ptDotDot then
  begin
    NextToken;
    Expression;
  end;
end;

procedure TmwSimplePasPar.SetIncludeHandler(IncludeHandler: IIncludeHandler);
begin
  FLexer.IncludeHandler := IncludeHandler;
end;

procedure TmwSimplePasPar.SetOnComment(const Value: TCommentEvent);
begin
  FLexer.OnComment := Value;
end;

procedure TmwSimplePasPar.QualifiedIdentifier;
begin
  Identifier;

  while TokenID = ptPoint do
  begin
    DotOp;
    Identifier;
  end;
end;

procedure TmwSimplePasPar.SetConstructor;
begin
  Expected(ptSquareOpen);
  if TokenID <> ptSquareClose then
  begin
    SetElement;
    while TokenID = ptComma do
    begin
      NextToken;
      SetElement;
    end;
  end;
  Expected(ptSquareClose);
end;

procedure TmwSimplePasPar.Number;
begin
  case TokenID of
    ptFloat:
      begin
        NextToken;
      end;
    ptIntegerConst:
      begin
        NextToken;
      end;
    ptIdentifier:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidNumber);
    end;
  end;
end;

procedure TmwSimplePasPar.ExpressionList;
begin
  Expression;
  if TokenID = ptAssign then
    begin
      Expected(ptAssign);
      Expression;
    end;
  while TokenID = ptComma do
  begin
    NextToken;
    Expression;
    if TokenID = ptAssign then
    begin
      Expected(ptAssign);
      Expression;
    end;
  end;
end;

procedure TmwSimplePasPar.Designator;
begin
  VariableReference;
end;

procedure TmwSimplePasPar.MultiplicativeOperator;
begin
  case TokenID of
    ptAnd:
      begin
        NextToken;
      end;
    ptDiv:
      begin
        NextToken;
      end;
    ptMod:
      begin
        NextToken;
      end;
    ptShl:
      begin
        NextToken;
      end;
    ptShr:
      begin
        NextToken;
      end;
    ptSlash:
      begin
        NextToken;
      end;
    ptStar:
      begin
        NextToken;
      end;
  else
    begin SynError(InvalidMultiplicativeOperator);
    end;
  end;
end;

procedure TmwSimplePasPar.Factor;
begin
  case TokenID of
    ptIf:
      begin
        TernaryOp;
      end;
    ptAsciiChar, ptStringConst:
      begin
        CharString;
      end;
    ptAddressOp, ptDoubleAddressOp, ptIdentifier, ptInherited, ptPointerSymbol:
      begin
        Designator;
      end;
    ptRoundOpen:
      begin
        RoundOpen;
        ExpressionList;
        RoundClose;
      end;
    ptIntegerConst, ptFloat:
      begin
        Number;
      end;
    ptNil:
      begin
        NilToken;
      end;
    ptMinus:
      begin
        UnaryMinus;
        Factor;
      end;
    ptNot:
      begin
        NotOp;
        Factor;
      end;
    ptPlus:
      begin
        NextToken;
        Factor;
      end;
    ptSquareOpen:
      begin
        SetConstructor;
      end;
    ptString:
      begin
        StringStatement;
      end;
    ptFunction, ptProcedure:
      AnonymousMethod;
  end;

  while TokenID = ptSquareOpen do
    IndexOp;

  while TokenID = ptPointerSymbol do
    PointerSymbol;

  if TokenID = ptRoundOpen then
    Factor;

  while TokenID = ptPoint do
  begin
    DotOp;
    Factor;
  end;
end;

procedure TmwSimplePasPar.AdditiveOperator;
begin
  if TokenID in [ptMinus, ptOr, ptPlus, ptXor] then
  begin
    NextToken;
  end
  else
  begin
    SynError(InvalidAdditiveOperator);
  end;
end;

procedure TmwSimplePasPar.AddressOp;
begin
  Expected(ptAddressOp);
end;

procedure TmwSimplePasPar.AlignmentParameter;
begin
  SimpleExpression;
end;

procedure TmwSimplePasPar.Term;
begin
  Factor;
  while TokenID in [ptAnd, ptDiv, ptMod, ptShl, ptShr, ptSlash, ptStar] do
  begin
    MultiplicativeOperator;
    Factor;
  end;
end;

procedure TmwSimplePasPar.RelativeOperator;
begin
  case TokenID of
    ptAs:
      begin
        NextToken;
      end;
    ptEqual:
      begin
        NextToken;
      end;
    ptGreater:
      begin
        NextToken;
      end;
    ptGreaterEqual:
      begin
        NextToken;
      end;
    ptIn:
      begin
        NextToken;
      end;
    ptIs:
      begin
        NextToken;
      end;
    ptLower:
      begin
        NextToken;
      end;
    ptLowerEqual:
      begin
        NextToken;
      end;
    ptNotEqual:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidRelativeOperator);
    end;
  end;
end;

procedure TmwSimplePasPar.SimpleExpression;
begin
  Term;
  while TokenID in [ptMinus, ptOr, ptPlus, ptXor] do
  begin
    AdditiveOperator;
    Term;
  end;

  case TokenID of
    ptAs:
      begin
        AsOp;
        TypeId;
      end;
  end;
end;

procedure TmwSimplePasPar.Expression;
begin
  SimpleExpression;

  //JT 2006-07-17 The Delphi language guide has this as
  //Expression -> SimpleExpression [RelOp SimpleExpression]...
  //So this needs to be able to repeat itself.
  case TokenID of
  ptEqual, ptGreater, ptGreaterEqual, ptLower, ptLowerEqual, ptIn,
    ptNotEqual, ptNot, ptIs:
    begin
      while TokenID in [ptEqual, ptGreater, ptGreaterEqual, ptLower, ptLowerEqual,
        ptIn, ptNotEqual{, ptColon}, ptNot, ptIs] do
      begin
        if TokenID = ptNot then
        begin
          Lexer.InitAhead;
          if Lexer.AheadTokenID = ptIn then
          begin
            NotInOp;
            SimpleExpression;
            Continue;
          end;
        end;

        if TokenID = ptIs then
        begin
          Lexer.InitAhead;
          if Lexer.AheadTokenID = ptNot then
          begin
            IsNotOp;
            SimpleExpression;
            Continue;
          end;
        end;

        RelativeOperator;
        SimpleExpression;
      end;
    end;
  ptColon:
    begin
      case InRound of
        False: ;
        True:
          while TokenID = ptColon do
          begin
            NextToken;
            AlignmentParameter;
          end;
      end;
    end;
  end;
end;

procedure TmwSimplePasPar.VarDeclaration;
begin
  VarNameList;
  Expected(ptColon);
  TypeKind;
  TypeDirective;

  case GenID of
    ptAbsolute:
      begin
        VarAbsolute;
      end;
    ptEqual:
      begin
        VarEqual;
      end;
  end;
  TypeDirective;
end;

procedure TmwSimplePasPar.VarAbsolute;
begin
  ExpectedEx(ptAbsolute);
  ConstantValue;
end;

procedure TmwSimplePasPar.VarEqual;
begin
  Expected(ptEqual);
  ConstantValueTyped;
end;

procedure TmwSimplePasPar.VarNameList;
begin
  VarName;
  while TokenID = ptComma do
    begin
      NextToken;
      VarName;
    end;
end;

procedure TmwSimplePasPar.VarName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.DirectiveCalling;
begin
  case ExID of
    ptCdecl:
      begin
        NextToken;
      end;
    ptPascal:
      begin
        NextToken;
      end;
    ptRegister:
      begin
        NextToken;
      end;
    ptSafeCall:
      begin
        NextToken;
      end;
    ptStdCall:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidDirectiveCalling);
    end;
  end;
end;

procedure TmwSimplePasPar.RecordVariant;
begin
  ConstantExpression;
  while (TokenID = ptComma) do
  begin
    NextToken;
    ConstantExpression;
  end;
  Expected(ptColon);
  Expected(ptRoundOpen);
  if TokenID <> ptRoundClose then
  begin
    FieldList;
  end;
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.VariantSection;
begin
  Expected(ptCase);
  TagField;
  Expected(ptOf);
  RecordVariant;
  while TokenID = ptSemiColon do
  begin
    Semicolon;
    case TokenID of
      ptEnd, ptRoundClose: Break;
    else
      RecordVariant;
    end;
  end;
end;

procedure TmwSimplePasPar.TagField;
begin
  TagFieldName;
  case FLexer.TokenID of
    ptColon:
      begin
        NextToken;
        TagFieldTypeName;
      end;
  end;
end;

procedure TmwSimplePasPar.TagFieldName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.TagFieldTypeName;
begin
  OrdinalType;
end;

procedure TmwSimplePasPar.FieldDeclaration;
begin
  if TokenID = ptSquareOpen then
    CustomAttribute;
  FieldNameList;
  Expected(ptColon);
  TypeKind;
  TypeDirective;
end;

procedure TmwSimplePasPar.FieldList;
begin
  while TokenID in [ptIdentifier, ptSquareOpen] do
  begin
    FieldDeclaration;
    Semicolon;
  end;
  if TokenID = ptCase then
  begin
    VariantSection;
  end;
end;

procedure TmwSimplePasPar.FieldName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.FieldNameList;
begin
  FieldName;
  while TokenID = ptComma do
  begin
    NextToken;
    FieldName;
  end;
end;

procedure TmwSimplePasPar.RecordType;
begin
  Expected(ptRecord);
  if TokenID = ptSemicolon then
    Exit;

  if ExID = ptHelper then
    ClassHelper;

  if TokenID = ptRoundOpen then
  begin
    ClassHeritage;
    if TokenID = ptSemicolon then
      Exit;
  end;
  ClassMemberList;
  Expected(ptEnd);

  ClassTypeEnd;
  RecordAlign;
end;

procedure TmwSimplePasPar.FileType;
begin
  Expected(ptFile);
  if TokenID = ptOf then
  begin
    NextToken;
    TypeId;
  end;
end;

procedure TmwSimplePasPar.FinalizationSection;
begin
  Expected(ptFinalization);
  StatementList;
end;

procedure TmwSimplePasPar.FinallyBlock;
begin
  StatementList;
end;

procedure TmwSimplePasPar.SetType;
begin
  Expected(ptSet);
  Expected(ptOf);
  OrdinalType;
end;

procedure TmwSimplePasPar.SetUseDefines(const Value: Boolean);
begin
  FLexer.UseDefines := Value;
end;

procedure TmwSimplePasPar.ArrayType;
begin
  Expected(ptArray);
  ArrayBounds;
  Expected(ptOf);
  TypeKind;
end;

procedure TmwSimplePasPar.EnumeratedType;
begin
  Expected(ptRoundOpen);
  EnumeratedTypeItem;
  while TokenID = ptComma do
  begin
    NextToken;
    EnumeratedTypeItem;
  end;
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.SubrangeType;
begin
  ConstantExpression;
  if TokenID = ptDotDot then
  begin
    NextToken;
    ConstantExpression;
  end;
end;

procedure TmwSimplePasPar.RealIdentifier;
begin
  case ExID of
    ptReal48:
      begin
        NextToken;
      end;
    ptReal:
      begin
        NextToken;
      end;
    ptSingle:
      begin
        NextToken;
      end;
    ptDouble:
      begin
        NextToken;
      end;
    ptExtended:
      begin
        NextToken;
      end;
    ptCurrency:
      begin
        NextToken;
      end;
    ptComp:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidRealIdentifier);
    end;
  end;
end;

procedure TmwSimplePasPar.RealType;
begin
  case TokenID of
    ptMinus:
      begin
        NextToken;
      end;
    ptPlus:
      begin
        NextToken;
      end;
  end;
  case TokenId of
    ptFloat:
      begin
        NextToken;
      end;
  else
    begin
      VariableReference;
    end;
  end;
end;

procedure TmwSimplePasPar.OrdinalIdentifier;
begin
  case ExID of
    ptBoolean:
      begin
        NextToken;
      end;
    ptByte:
      begin
        NextToken;
      end;
    ptBytebool:
      begin
        NextToken;
      end;
    ptCardinal:
      begin
        NextToken;
      end;
    ptChar:
      begin
        NextToken;
      end;
    ptDWord:
      begin
        NextToken;
      end;
    ptInt64:
      begin
        NextToken;
      end;
    ptInteger:
      begin
        NextToken;
      end;
    ptLongBool:
      begin
        NextToken;
      end;
    ptLongInt:
      begin
        NextToken;
      end;
    ptLongWord:
      begin
        NextToken;
      end;
    ptPChar:
      begin
        NextToken;
      end;
    ptShortInt:
      begin
        NextToken;
      end;
    ptSmallInt:
      begin
        NextToken;
      end;
    ptWideChar:
      begin
        NextToken;
      end;
    ptWord:
      begin
        NextToken;
      end;
    ptWordbool:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidOrdinalIdentifier);
    end;
  end;
end;

procedure TmwSimplePasPar.OrdinalType;
begin
  case TokenID of
    ptIdentifier:
      begin
        Lexer.InitAhead;
        case Lexer.AheadTokenID of
          ptPoint:
            begin
              TypeId;
            end;
          ptRoundOpen, ptDotDot:
            begin
              ConstantExpression;
            end;
        else
          begin
            TypeID;
          end;
        end;
      end;
    ptRoundOpen:
      begin
        EnumeratedType;
      end;
    ptSquareOpen:
      begin
        NextToken;
        SubrangeType;
        Expected(ptSquareClose);
      end;
  else
    begin
      ConstantExpression;
    end;
  end;
  if TokenID = ptDotDot then
  begin
    NextToken;
    ConstantExpression;
  end;
end;

procedure TmwSimplePasPar.VariableReference;
begin
  case TokenID of
    ptRoundOpen:
    begin
      RoundOpen;
      Expression;
      RoundClose;
      VariableTail;
    end;
    ptSquareOpen:
    begin
      SetConstructor;
    end;
    ptAddressOp:
      begin
        AddressOp;
        VariableReference;
      end;
    ptDoubleAddressOp:
      begin
        NextToken;
        VariableReference;
      end;
    ptInherited:
      begin
        InheritedVariableReference;
      end;
  else
    variable;
  end;
end;

procedure TmwSimplePasPar.Variable; (* Attention: could also came from proc_call ! ! *)
begin
  QualifiedIdentifier;
  VariableTail;
end;

procedure TmwSimplePasPar.VariableTail;
begin
  case TokenID of
    ptRoundOpen:
      begin
        RoundOpen;
        ExpressionList;
        RoundClose;
      end;
    ptSquareOpen:
      begin
        IndexOp;
      end;
    ptPointerSymbol:
      begin
        PointerSymbol;
      end;
    ptLower:
      begin
        InitAhead;
        AheadParse.NextToken;
        AheadParse.TypeArgs;

        if AheadParse.TokenId = ptGreater then
        begin
          NextToken;
          TypeArgs;
          Expected(ptGreater);
          case TokenID of
            ptAddressOp, ptDoubleAddressOp, ptIdentifier:
              begin
                VariableReference;
              end;
            ptPoint, ptPointerSymbol, ptRoundOpen, ptSquareOpen:
              begin
                VariableTail;
              end;
          end;
        end;
      end;
  end;

  case TokenID of
    ptRoundOpen, ptSquareOpen, ptPointerSymbol:
    begin
      VariableTail;
    end;
    ptPoint:
    begin
      DotOp;
      Variable;
    end;
    ptAs:
    begin
      AsOp;
      SimpleExpression;
    end;
  end;
end;

procedure TmwSimplePasPar.InterfaceType;
begin
  case TokenID of
    ptInterface:
      begin
        NextToken;
      end;
    ptDispInterface:
      begin
        NextToken;
      end
  else
    begin
      SynError(InvalidInterfaceType);
    end;
  end;
  case TokenID of
    ptEnd:
      begin
        NextToken; { Direct descendant without new members }
      end;
    ptRoundOpen:
      begin
        InterfaceHeritage;
        case TokenID of
          ptEnd:
            begin
              NextToken; { No new members }
            end;
          ptSemiColon: ; { No new members }
        else
          begin
            if TokenID = ptSquareOpen then
            begin
              InterfaceGUID;
            end;
            InterfaceMemberList;
            Expected(ptEnd);
          end;
        end;
      end;
  else
    begin
      if TokenID = ptSquareOpen then
      begin
        InterfaceGUID;
      end;
      InterfaceMemberList; { Direct descendant }
      Expected(ptEnd);
    end;
  end;
end;

procedure TmwSimplePasPar.InterfaceMemberList;
begin
  while TokenID in [ptSquareOpen, ptFunction, ptProcedure, ptProperty] do
  begin
    while TokenID = ptSquareOpen do
      CustomAttribute;

    ClassMethodOrProperty;
  end;
end;

procedure TmwSimplePasPar.ClassType;
begin
  Expected(ptClass);
  case TokenID of
    ptIdentifier: //NASTY hack because Abstract is generally an ExID, except in this case when it should be a keyword.
      begin
        if Lexer.ExID = ptAbstract then
          Expected(ptIdentifier);

        if Lexer.ExID = ptHelper then
          ClassHelper;
      end;
    ptSealed:
      Expected(ptSealed);
  end;
  case TokenID of
    ptEnd:
      begin
        ClassTypeEnd;
        NextToken; { Direct descendant of TObject without new members }
      end;
    ptRoundOpen:
      begin
        ClassHeritage;
        case TokenID of
          ptEnd:
            begin
              Expected(ptEnd);
              ClassTypeEnd;
            end;
          ptSemiColon: ClassTypeEnd;
        else
          begin
            ClassMemberList; { Direct descendant of TObject }
            Expected(ptEnd);
            ClassTypeEnd;
          end;
        end;
      end;
    ptSemicolon: ClassTypeEnd;
  else
    begin
      ClassMemberList; { Direct descendant of TObject }
      Expected(ptEnd);
      ClassTypeEnd;
    end;
  end;
end;

procedure TmwSimplePasPar.ClassHelper;
begin
  ExpectedEx(ptHelper);
  if TokenID = ptRoundOpen then
    ClassHeritage;
  Expected(ptFor);
  TypeId;
end;

procedure TmwSimplePasPar.ClassHeritage;
begin
  Expected(ptRoundOpen);
  AncestorIdList;
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.ClassVisibility;
var
  IsStrict: boolean;
begin
  IsStrict := ExID = ptStrict;
  if IsStrict then
    ExpectedEx(ptStrict);

  while ExID in [ptAutomated, ptPrivate, ptProtected, ptPublic, ptPublished] do
  begin
    Lexer.InitAhead;
    case Lexer.AheadExID of
      ptColon, ptComma: ;
    else
      case ExID of
        ptAutomated:
          begin
            VisibilityAutomated;
          end;
        ptPrivate:
          begin
            if IsStrict then
              VisibilityStrictPrivate
            else
              VisibilityPrivate;
          end;
        ptProtected:
          begin
            if IsStrict then
              VisibilityStrictProtected
            else
              VisibilityProtected;
          end;
        ptPublic:
          begin
            VisibilityPublic;
          end;
        ptPublished:
          begin
            VisibilityPublished;
          end;
      end;
    end;
  end;
end;

procedure TmwSimplePasPar.VisibilityAutomated;
begin
  ExpectedEx(ptAutomated);
end;

procedure TmwSimplePasPar.VisibilityStrictPrivate;
begin
  ExpectedEx(ptPrivate);
end;

procedure TmwSimplePasPar.VisibilityPrivate;
begin
  ExpectedEx(ptPrivate);
end;

procedure TmwSimplePasPar.VisibilityStrictProtected;
begin
  ExpectedEx(ptProtected);
end;

procedure TmwSimplePasPar.VisibilityProtected;
begin
  ExpectedEx(ptProtected);
end;

procedure TmwSimplePasPar.VisibilityPublic;
begin
  ExpectedEx(ptPublic);
end;

procedure TmwSimplePasPar.VisibilityPublished;
begin
  ExpectedEx(ptPublished);
end;

procedure TmwSimplePasPar.VisibilityUnknown;
begin
end;

procedure TmwSimplePasPar.ClassMemberList;
begin
  while (TokenID in [ptClass, ptConstructor, ptDestructor, ptFunction,
    ptIdentifier, ptProcedure, ptProperty, ptType, ptSquareOpen, ptVar, ptConst, ptCase]) or (ExID = ptStrict) do
  begin
    ClassVisibility;

    if TokenID = ptSquareOpen then
      CustomAttribute;

    if (TokenID = ptIdentifier) and
      not (ExID in [ptPrivate, ptProtected, ptPublished, ptPublic, ptStrict]) then
    begin
      InitAhead;
      AheadParse.NextToken;

      if AheadParse.TokenId = ptEqual then
        ConstantDeclaration
      else
      begin
        ClassField;
        if TokenID = ptEqual then
        begin
          NextToken;
          TypedConstant;
        end;
      end;

      Semicolon;
    end
    else if TokenID in [ptClass, ptConstructor, ptDestructor, ptFunction,
      ptProcedure, ptProperty, ptVar, ptConst] then
    begin
      ClassMethodOrProperty;
    end;
    if TokenID = ptType then
      TypeSection;
    if TokenID = ptCase then
    begin
      VariantSection;
    end;
  end;
end;

procedure TmwSimplePasPar.ClassMethodOrProperty;
var
  CurToken: TptTokenKind;
begin
  if TokenID = ptClass then
  begin
    InitAhead;
    AheadParse.NextToken;
    CurToken := AheadParse.TokenID;
  end else
    CurToken := TokenID;

  case CurToken of
    ptProperty:
      begin
        ClassProperty;
      end;
    ptVar, ptThreadVar:
      begin
        if TokenID = ptClass then
          ClassClass;

        NextToken;
        while (TokenID = ptIdentifier) and (ExID = ptUnknown) do
        begin
          ClassField;
          Semicolon;
        end;
      end;
    ptConst:
      begin
        if TokenID = ptClass then
          ClassClass;

        NextToken;
        while (TokenID = ptIdentifier) and (ExID = ptUnknown) do
        begin
          ConstantDeclaration;
          Semicolon;
        end;
      end;
  else
    begin
      ClassMethodHeading;
    end;
  end;
end;

procedure TmwSimplePasPar.ClassProperty;
begin
  if TokenID = ptClass then
    ClassClass;

  Expected(ptProperty);
  PropertyName;
  case TokenID of
    ptColon, ptSquareOpen:
      begin
        PropertyInterface;
      end;
  end;
  PropertySpecifiers;
  case ExID of
    ptDefault:
      begin
        PropertyDefault;
        Semicolon;
      end;
  end;
end;

procedure TmwSimplePasPar.PropertyName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.ClassField;
begin
  if TokenID = ptSquareOpen then
    CustomAttribute;
  FieldNameList;
  Expected(ptColon);
  TypeKind;
  TypeDirective;
end;

procedure TmwSimplePasPar.ObjectType;
begin
  Expected(ptObject);
  case TokenID of
    ptEnd:
      begin
        ObjectTypeEnd;
        NextToken; { Direct descendant without new members }
      end;
    ptRoundOpen:
      begin
        ObjectHeritage;
        case TokenID of
          ptEnd:
            begin
              Expected(ptEnd);
              ObjectTypeEnd;
            end;
          ptSemiColon: ObjectTypeEnd;
        else
          begin
            ObjectMemberList; { Direct descendant }
            Expected(ptEnd);
            ObjectTypeEnd;
          end;
        end;
      end;
  else
    begin
      ObjectMemberList; { Direct descendant }
      Expected(ptEnd);
      ObjectTypeEnd;
    end;
  end;
end;

procedure TmwSimplePasPar.ObjectHeritage;
begin
  Expected(ptRoundOpen);
  AncestorIdList;
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.ObjectMemberList;
begin {jdj added ptProperty-call to ObjectProperty 02/07/2001}
  ObjectVisibility;
  while TokenID in [ptConstructor, ptDestructor, ptFunction, ptIdentifier,
    ptProcedure, ptProperty] do
  begin
    while TokenID = ptIdentifier do
    begin
      ObjectField;
      Semicolon;
      ObjectVisibility;
    end;
    while TokenID in [ptConstructor, ptDestructor, ptFunction, ptProcedure, ptProperty] do
    begin
      case TokenID of
        ptConstructor, ptDestructor, ptFunction, ptProcedure:
          ObjectMethodHeading;
        ptProperty:
          ObjectProperty;
      end;
    end;
    ObjectVisibility;
  end;
end;

procedure TmwSimplePasPar.ObjectVisibility;
begin
  while ExID in [ptPrivate, ptProtected, ptPublic] do
  begin
    Lexer.InitAhead;
    case Lexer.AheadExID of
      ptColon, ptComma: ;
    else
      case ExID of
        ptPrivate:
          begin
            VisibilityPrivate;
          end;
        ptProtected:
          begin
            VisibilityProtected;
          end;
        ptPublic:
          begin
            VisibilityPublic;
          end;
      end;
    end;
  end;
end;

procedure TmwSimplePasPar.ObjectField;
begin
  IdentifierList;
  Expected(ptColon);
  TypeKind;
  TypeDirective;
end;

procedure TmwSimplePasPar.ClassReferenceType;
begin
  Expected(ptClass);
  Expected(ptOf);
  TypeId;
end;

procedure TmwSimplePasPar.VariantIdentifier;
begin
  case ExID of
    ptOleVariant:
      begin
        NextToken;
      end;
    ptVariant:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidVariantIdentifier);
    end;
  end;
end;

procedure TmwSimplePasPar.ProceduralType;
var
  TheTokenID: TptTokenKind;
begin
  case TokenID of
    ptFunction:
      begin
        NextToken;
        if TokenID = ptRoundOpen then
        begin
          FormalParameterList;
        end;
        Expected(ptColon);
        ReturnType;
      end;
    ptProcedure:
      begin
        NextToken;
        if TokenID = ptRoundOpen then
        begin
          FormalParameterList;
        end;
      end;
  else
    begin
      SynError(InvalidProceduralType);
    end;
  end;
  if TokenID = ptOf then
    ProceduralDirectiveOf;

  Lexer.InitAhead;
  case TokenID of
    ptSemiColon: TheTokenID := Lexer.AheadExID;
  else
    TheTokenID := ExID;
  end;
  while TheTokenID in [ptAbstract, ptCdecl, ptDynamic, ptExport, ptExternal, ptFar,
    ptMessage, ptNear, ptOverload, ptOverride, ptPascal, ptRegister,
    ptReintroduce, ptSafeCall, ptStdCall, ptVirtual, ptStatic, ptInline, ptVarargs, ptNoreturn] do
  // DR 2001-11-14 no checking for deprecated etc. since it's captured by the typedecl
  begin
    if TokenID = ptSemiColon then Semicolon;
    ProceduralDirective;
    Lexer.InitAhead;
    case TokenID of
      ptSemiColon: TheTokenID := Lexer.AheadExID;
    else
      TheTokenID := ExID;
    end;
  end;

  if TokenID = ptOf then
    ProceduralDirectiveOf;
end;

procedure TmwSimplePasPar.StringConst;
begin
  StringConstSimple;
  while TokenID in [ptStringConst, ptAsciiChar] do
    StringConstSimple;
end;

procedure TmwSimplePasPar.StringConstSimple;
begin
  NextToken;
end;

procedure TmwSimplePasPar.StringIdentifier;
begin
  case ExID of
    ptAnsiString:
      begin
        NextToken;
      end;
    ptShortString:
      begin
        NextToken;
      end;
    ptWideString:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidStringIdentifier);
    end;
  end;
end;

procedure TmwSimplePasPar.StringType;
begin
  case TokenID of
    ptString:
      begin
        NextToken;
        if TokenID = ptSquareOpen then
        begin
          NextToken;
          ConstantExpression;
          Expected(ptSquareClose);
        end;
      end;
  else
    begin
      VariableReference;
    end;
  end;
end;

procedure TmwSimplePasPar.PointerSymbol;
begin
  Expected(ptPointerSymbol);
end;

procedure TmwSimplePasPar.PointerType;
begin
  Expected(ptPointerSymbol);
  TypeId;
end;

procedure TmwSimplePasPar.StructuredType;
begin
  case TokenID of
    ptArray:
      begin
        ArrayType;
      end;
    ptFile:
      begin
        FileType;
      end;
    ptRecord:
      begin
        RecordType;
      end;
    ptSet:
      begin
        SetType;
      end;
    ptObject:
      begin
        ObjectType;
      end
  else
    begin
      SynError(InvalidStructuredType);
    end;
  end;
end;

procedure TmwSimplePasPar.SimpleType;
begin
  case TokenID of
    ptMinus:
      begin
        NextToken;
      end;
    ptPlus:
      begin
        NextToken;
      end;
  end;
  case FLexer.TokenID of
    ptAsciiChar, ptIntegerConst:
      begin
        OrdinalType;
      end;
    ptFloat:
      begin
        RealType;
      end;
    ptIdentifier:
      begin
        InitAhead;
        AheadParse.NextToken;
        AheadParse.SimpleExpression;
        if AheadParse.TokenID = ptDotDot then
          SubrangeType
        else
          TypeId;
      end;
  else
    begin
      VariableReference;
    end;
  end;
end;

procedure TmwSimplePasPar.RecordAlign;
begin
  if ExID = ptAlign then
  begin
    NextToken;
    RecordAlignValue;
  end;
end;

procedure TmwSimplePasPar.RecordAlignValue;
begin
  Expected(ptIntegerConst);
end;

procedure TmwSimplePasPar.RecordFieldConstant;
begin
  Expected(ptIdentifier);
  Expected(ptColon);
  TypedConstant;
end;

procedure TmwSimplePasPar.RecordConstant;
begin
  Expected(ptRoundOpen);
  RecordFieldConstant;
  while (TokenID = ptSemiColon) do
  begin
    Semicolon;
    if TokenId <> ptRoundClose then
      RecordFieldConstant;
  end;
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.RecordConstraint;
begin
  Expected(ptRecord);
end;

procedure TmwSimplePasPar.ArrayConstant;
begin
  Expected(ptRoundOpen);

  TypedConstant;
  if TokenID = ptDotDot then
  begin
    NextToken;
    TypedConstant;
  end;

  while (TokenID = ptComma) do
  begin
    NextToken;
    TypedConstant;
    if TokenID = ptDotDot then
    begin
      NextToken;
      TypedConstant;
    end;
  end;
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.ArrayDimension;
begin
  OrdinalType;
end;

procedure TmwSimplePasPar.ClassForward;
begin
  Expected(ptClass);
end;

procedure TmwSimplePasPar.DispInterfaceForward;
begin
  Expected(ptDispInterface);
end;

procedure TmwSimplePasPar.DotOp;
begin
  Expected(ptPoint);
end;

procedure TmwSimplePasPar.InterfaceForward;
begin
  Expected(ptInterface);
end;

procedure TmwSimplePasPar.ObjectForward;
begin
  Expected(ptObject);
end;

procedure TmwSimplePasPar.TypeDeclaration;
begin
  TypeName;
  Expected(ptEqual);

  Lexer.InitAhead;

  if TokenID = ptType then
  begin
    if Lexer.AheadTokenID = ptOf then
    begin
      TypeReferenceType;
      TypeDirective;
      Exit;
    end else
      ExplicitType;
  end;

  if (TokenID = ptPacked) and (Lexer.AheadTokenID in [ptClass, ptObject]) then
    NextToken;

  case TokenID of
    ptPointerSymbol:
      begin
        PointerType;
      end;
    ptClass:
      begin
        case Lexer.AheadTokenID of
          ptOf:
            begin
              ClassReferenceType;
            end;
          ptSemiColon:
            begin
              ClassForward;
            end;
        else
          begin
            ClassType;
          end;
        end;
      end;
    ptInterface:
      begin
        case Lexer.AheadTokenID of
          ptSemiColon:
            begin
              InterfaceForward;
            end;
        else
          begin
            InterfaceType;
          end;
        end;
      end;
    ptDispInterface:
      begin
        case Lexer.AheadTokenID of
          ptSemiColon:
            begin
              DispInterfaceForward;
            end;
        else
          begin
            InterfaceType;
          end;
        end;
      end;
    ptObject:
      begin
        case Lexer.AheadTokenID of
          ptSemiColon:
            begin
              ObjectForward;
            end;
        else
          begin
            ObjectType;
          end;
        end;
      end;
  else
    begin
      if ExID = ptReference then
        AnonymousMethodType
      else
        TypeKind;
    end;
  end;
  TypeDirective;
end;

procedure TmwSimplePasPar.TypeName;
begin
  Expected(ptIdentifier);
  if TokenId = ptLower then
    TypeParams;
end;

procedure TmwSimplePasPar.ExplicitType;
begin
  Expected(ptType);
end;

procedure TmwSimplePasPar.TypeKind;
begin
  case TokenID of
    ptAsciiChar, ptFloat, ptIntegerConst, ptMinus, ptNil, ptPlus, ptStringConst, ptConst:
      begin
        SimpleType;
      end;
    ptRoundOpen:
      begin
        EnumeratedType;
      end;
    ptSquareOpen:
      begin
        SubrangeType;
      end;
    ptArray, ptFile, ptPacked, ptRecord, ptSet:
      begin
        if TokenID = ptPacked then
          NextToken;
        StructuredType;
      end;
    ptFunction, ptProcedure:
      begin
        ProceduralType;
      end;
    ptIdentifier:
      begin
        InitAhead;
        AheadParse.NextToken;
        AheadParse.SimpleExpression;
        if AheadParse.TokenID = ptDotDot then
          SubrangeType
        else
          TypeId;
      end;
    ptPointerSymbol:
      begin
        PointerType;
      end;
    ptString:
      begin
        TypeId;
      end;
  else
    begin
      SynError(InvalidTypeKind);
    end;
  end;
end;

procedure TmwSimplePasPar.TypeArgs;
begin
  TypeKind;
  while TokenId = ptComma do
  begin
    NextToken;
    TypeKind;
  end;
end;

procedure TmwSimplePasPar.TypedConstant;
var
  RoundBrackets: Integer;
begin
  case TokenID of
    ptRoundOpen:
      begin
        Lexer.InitAhead;
        while Lexer.AheadTokenID <> ptSemiColon do
          case Lexer.AheadTokenID of
            ptAnd, ptBegin, ptCase, ptColon, ptEnd, ptElse, ptIf, ptMinus, ptNull,
              ptOr, ptPlus, ptShl, ptShr, ptSlash, ptStar, ptWhile, ptWith,
              ptXor: Break;
            ptRoundOpen:
              begin
                RoundBrackets := 0;
                repeat
                  case Lexer.AheadTokenID of
                    ptBegin, ptCase, ptEnd, ptElse, ptIf, ptNull, ptWhile, ptWith: Break;
                  else
                    if Lexer.AheadTokenID = ptRoundOpen then
                      Inc(RoundBrackets);
                    if Lexer.AheadTokenID = ptRoundClose then
                      Dec(RoundBrackets);

                    Lexer.AheadNext;
                  end;
                until RoundBrackets = 0;
              end;
          else
            Lexer.AheadNext;
          end;
        case Lexer.AheadTokenID of
          ptColon:
            begin
              RecordConstant;
            end;
          ptNull: ;
          ptAnd, ptMinus, ptOr, ptPlus, ptShl, ptShr, ptSlash, ptStar, ptXor:
            begin
              ConstantExpression;
            end;
        else
          begin
            ArrayConstant;
          end;
        end;
      end;
    ptSquareOpen:
      ConstantExpression;
  else
    begin
      ConstantExpression;
    end;
  end;
end;

procedure TmwSimplePasPar.TypeId;
begin
  TypeSimple;

  while TokenID = ptPoint do
  begin
    Expected(ptPoint);
    TypeSimple;
  end;

  if TokenID = ptRoundOpen then
  begin
    Expected(ptRoundOpen);
    SimpleExpression;
    Expected(ptRoundClose);
  end;
end;

procedure TmwSimplePasPar.ConstantExpression;
begin
  SimpleExpression;
end;

procedure TmwSimplePasPar.ResourceDeclaration;
begin
  ConstantName;
  Expected(ptEqual);

  ResourceValue;

  TypeDirective;
end;

procedure TmwSimplePasPar.ResourceValue;
begin
  CharString;
  while TokenID = ptPlus do
  begin
    NextToken;
    CharString;
  end;
end;

procedure TmwSimplePasPar.ConstantDeclaration;
begin
  ConstantName;
  case TokenID of
    ptEqual:
      begin
        ConstantEqual;
      end;
    ptColon:
      begin
        ConstantColon;
      end;
  else
    begin
      SynError(InvalidConstantDeclaration);
    end;
  end;
  TypeDirective;
end;

procedure TmwSimplePasPar.ConstantColon;
begin
  Expected(ptColon);
  ConstantType;
  Expected(ptEqual);
  ConstantValueTyped;
end;

procedure TmwSimplePasPar.ConstantEqual;
begin
  Expected(ptEqual);
  ConstantValue;
end;

procedure TmwSimplePasPar.ConstantValue;
begin
  Expression;
end;

procedure TmwSimplePasPar.ConstantValueTyped;
begin
  TypedConstant;
end;

procedure TmwSimplePasPar.ConstantName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.ConstantType;
begin
  TypeKind;
end;

procedure TmwSimplePasPar.LabelId;
begin
  case TokenID of
    ptIntegerConst:
      begin
        NextToken;
      end;
    ptIdentifier:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidLabelId);
    end;
  end;
end;

procedure TmwSimplePasPar.ProcedureDeclarationSection;
begin
  if TokenID = ptClass then
  begin
    ClassMethod;
  end;
  case TokenID of
    ptConstructor:
      begin
        ProcedureProcedureName;
      end;
    ptDestructor:
      begin
        ProcedureProcedureName;
      end;
    ptProcedure:
      begin
        ProcedureProcedureName;
      end;
    ptFunction:
      begin
        FunctionMethodDeclaration;
      end;
    ptIdentifier:
      begin
        if Lexer.ExID = ptOperator then
        begin
          FunctionMethodDeclaration;
        end
        else
          SynError(InvalidProcedureDeclarationSection);
      end;
  else
    begin
      SynError(InvalidProcedureDeclarationSection);
    end;
  end;
end;

procedure TmwSimplePasPar.LabelDeclarationSection;
begin
  Expected(ptLabel);
  LabelId;
  while (TokenID = ptComma) do
  begin
    NextToken;
    LabelId;
  end;
  Semicolon;
end;

procedure TmwSimplePasPar.ProceduralDirective;
begin
  case GenID of
    ptAbstract:
      begin
        DirectiveBinding;
      end;
    ptCdecl, ptPascal, ptRegister, ptSafeCall, ptStdCall:
      begin
        DirectiveCalling;
      end;
    ptExport, ptFar, ptNear:
      begin
        Directive16Bit;
      end;
    ptExternal:
      begin
        ExternalDirective;
      end;
    ptDynamic, ptMessage, ptOverload, ptOverride, ptReintroduce, ptVirtual, ptNoreturn:
      begin
        DirectiveBinding;
      end;
    ptAssembler:
      begin
        NextToken;
      end;
    ptStatic:
      begin
        NextToken;
      end;
    ptInline:
      begin
        DirectiveInline;
      end;
    ptDeprecated:
      DirectiveDeprecated;
    ptLibrary:
      DirectiveLibrary;
    ptPlatform:
      DirectivePlatform;
    ptLocal:
      DirectiveLocal;
    ptVarargs:
      DirectiveVarargs;
    ptFinal, ptExperimental, ptDelayed:
      NextToken;
  else
    begin
      SynError(InvalidProceduralDirective);
    end;
  end;
end;

procedure TmwSimplePasPar.ExportedHeading;
begin
  case TokenID of
    ptFunction:
      begin
        FunctionHeading;
      end;
    ptProcedure:
      begin
        ProcedureHeading;
      end;
  else
    begin
      SynError(InvalidExportedHeading);
    end;
  end;
  if TokenID = ptSemiColon then Semicolon;

  //TODO: Add FINAL
  while ExID in [ptAbstract, ptCdecl, ptDynamic, ptExport, ptExternal, ptFar,
    ptMessage, ptNear, ptOverload, ptOverride, ptPascal, ptRegister,
    ptReintroduce, ptSafeCall, ptStdCall, ptVirtual,
    ptDeprecated, ptLibrary, ptPlatform, ptLocal, ptVarargs,
    ptStatic, ptInline, ptAssembler, ptForward, ptDelayed, ptNoreturn] do
  begin
    case ExID of
      ptAssembler: NextToken;
      ptForward: ForwardDeclaration;
    else
      ProceduralDirective;
    end;
    if TokenID = ptSemiColon then Semicolon;
  end;
end;

procedure TmwSimplePasPar.FunctionHeading;
begin
  Expected(ptFunction);
  FunctionProcedureName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  if TokenID = ptColon then
  begin
    Expected(ptColon);
    ReturnType;
  end;
end;

procedure TmwSimplePasPar.ProcedureHeading;
begin
  Expected(ptProcedure);
  FunctionProcedureName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
end;

procedure TmwSimplePasPar.VarSection;
begin
  case TokenID of
    ptThreadVar:
      begin
        NextToken;
      end;
    ptVar:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidVarSection);
    end;
  end;
  while TokenID in [ptIdentifier, ptSquareOpen] do
  begin
    if TokenID = ptSquareOpen then
      CustomAttribute
    else
    begin
      VarDeclaration;
      Semicolon;
    end;
  end;
end;

procedure TmwSimplePasPar.TypeSection;
begin
  Expected(ptType);

  while (TokenID = ptIdentifier) or (Lexer.TokenID = ptSquareOpen) do
  begin
    if TokenID = ptSquareOpen then
      CustomAttribute
    else
    begin
      InitAhead;
      AheadParse.NextToken;
      if AheadParse.TokenID = ptLower then
        AheadParse.TypeParams;

      if AheadParse.TokenID <> ptEqual then
        Break;

      TypeDeclaration;
      if TokenID = ptEqual then
        TypedConstant;
      Semicolon;
    end;
  end;
end;

procedure TmwSimplePasPar.TypeSimple;
begin
  case GenID of
    ptBoolean, ptByte, ptChar, ptDWord, ptInt64, ptInteger, ptLongInt,
      ptLongWord, ptPChar, ptShortInt, ptSmallInt, ptWideChar, ptWord:
      begin
        OrdinalIdentifier;
      end;
    ptComp, ptCurrency, ptDouble, ptExtended, ptReal, ptReal48, ptSingle:
      begin
        RealIdentifier;
      end;
    ptAnsiString, ptShortString, ptWideString:
      begin
        StringIdentifier;
      end;
    ptOleVariant, ptVariant:
      begin
        VariantIdentifier;
      end;
    ptString:
      begin
        StringType;
      end;
    ptFile:
      begin
        FileType;
      end;
    ptArray:
      begin
        NextToken;
        Expected(ptOf);
        case TokenID of
          ptConst: (*new in ObjectPascal80*)
            begin
              NextToken;
            end;
        else
          begin
            TypeID;
          end;
        end;
      end;
  else
    Expected(ptIdentifier);
  end;

  if TokenId = ptLower then
  begin
    Expected(ptLower);
    TypeArgs;
    Expected(ptGreater);
  end;
end;

procedure TmwSimplePasPar.TypeParamDecl;
begin
  TypeParamList;
  if TokenId = ptColon then
  begin
    NextToken;
    ConstraintList;
  end;
end;

procedure TmwSimplePasPar.TypeParamDeclList;
begin
  TypeParamDecl;
  while TokenId = ptSemicolon do
  begin
    NextToken;
    TypeParamDecl;
  end;
end;

procedure TmwSimplePasPar.TypeParamList;
begin
  if TokenId = ptSquareOpen then
    AttributeSection;
  TypeSimple;
  while TokenId = ptComma do
  begin
    NextToken;
    if TokenId = ptSquareOpen then
      AttributeSection;
    TypeSimple;
  end;
end;

procedure TmwSimplePasPar.TypeParams;
begin
  Expected(ptLower);
  TypeParamDeclList;
  // workaround for TSomeClass< T >= class(TObject)
  if TokenID = ptGreaterEqual then
    Lexer.RunPos := Lexer.RunPos - 1
  else
    Expected(ptGreater);
end;

procedure TmwSimplePasPar.TypeReferenceType;
begin
  Expected(ptType);
  Expected(ptOf);
  TypeId;
end;

procedure TmwSimplePasPar.ConstSection;
begin
  case TokenID of
    ptConst:
      begin
        NextToken;
        while TokenID in [ptIdentifier, ptSquareOpen] do
        begin
          if TokenID = ptSquareOpen then
            CustomAttribute
          else
          begin
            ConstantDeclaration;
            Semicolon;
          end;
        end;
      end;
    ptResourceString:
      begin
        NextToken;
        while (TokenID = ptIdentifier) do
        begin
          ResourceDeclaration;
          Semicolon;
        end;
      end
  else
    begin
      SynError(InvalidConstSection);
    end;
  end;
end;

procedure TmwSimplePasPar.InterfaceDeclaration;
begin
  case TokenID of
    ptConst:
      begin
        ConstSection;
      end;
    ptFunction:
      begin
        ExportedHeading;
      end;
    ptProcedure:
      begin
        ExportedHeading;
      end;
    ptResourceString:
      begin
        ConstSection;
      end;
    ptType:
      begin
        TypeSection;
      end;
    ptThreadVar:
      begin
        VarSection;
      end;
    ptVar:
      begin
        VarSection;
      end;
    ptExports:
      begin
        ExportsClause;
      end;
    ptSquareOpen:
      begin
        CustomAttribute;
      end;
  else
    begin
      SynError(InvalidInterfaceDeclaration);
    end;
  end;
end;

procedure TmwSimplePasPar.ExportsElement;
begin
  ExportsName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;

  if FLexer.ExID = ptIndex then
  begin
    NextToken;
    Expected(ptIntegerConst);
  end;
  if FLexer.ExID = ptName then
  begin
    NextToken;
    SimpleExpression;
  end;
  if FLexer.ExID = ptResident then
  begin
    NextToken;
  end;
end;

procedure TmwSimplePasPar.CompoundStatement;
begin
  Expected(ptBegin);
  Statements;
  Expected(ptEnd);
end;

procedure TmwSimplePasPar.ExportsClause;
begin
  Expected(ptExports);
  ExportsElement;
  while TokenID = ptComma do
  begin
    NextToken;
    ExportsElement;
  end;
  Semicolon;
end;

procedure TmwSimplePasPar.ContainsClause;
begin
  ExpectedEx(ptContains);
  MainUsedUnitStatement;
  while TokenID = ptComma do
  begin
    NextToken;
    MainUsedUnitStatement;
  end;
  Semicolon;
end;

procedure TmwSimplePasPar.RequiresClause;
begin
  ExpectedEx(ptRequires);
  RequiresIdentifier;
  while TokenID = ptComma do
  begin
    NextToken;
    RequiresIdentifier;
  end;
  Semicolon;
end;

procedure TmwSimplePasPar.RequiresIdentifier;
begin
  RequiresIdentifierId;
  while Lexer.TokenID = ptPoint do
  begin
    NextToken;
    RequiresIdentifierId;
  end;
end;

procedure TmwSimplePasPar.RequiresIdentifierId;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.InitializationSection;
begin
  Expected(ptInitialization);
  StatementList;
end;

procedure TmwSimplePasPar.ImplementationSection;
begin
  Expected(ptImplementation);
  if TokenID = ptUses then
  begin
    UsesClause;
  end;
  while TokenID in [ptClass, ptConst, ptConstructor, ptDestructor, ptFunction,
    ptLabel, ptProcedure, ptResourceString, ptThreadVar, ptType, ptVar,
    ptExports, ptSquareOpen] do
  begin
    DeclarationSection;
  end;
end;

procedure TmwSimplePasPar.InterfaceSection;
begin
  Expected(ptInterface);
  if TokenID = ptUses then
  begin
    UsesClause;
  end;
  while TokenID in [ptConst, ptFunction, ptResourceString, ptProcedure,
    ptThreadVar, ptType, ptVar, ptExports, ptSquareOpen] do
  begin
    InterfaceDeclaration;
  end;
end;

procedure TmwSimplePasPar.IdentifierList;
begin
  Identifier;
  while TokenID = ptComma do
  begin
    NextToken;
    Identifier;
  end;
end;

procedure TmwSimplePasPar.CharString;
begin
  case GenID of
    ptAsciiChar, ptIdentifier, ptRoundOpen, ptStringConst:
      while GenID in
        [ptAsciiChar, ptIdentifier, ptRoundOpen, ptStringConst, ptString] do
      begin
        case TokenID of
          ptIdentifier, ptRoundOpen:
            begin
              if ExID in [ptIndex] then
                Break;
              VariableReference;
            end;
          ptString:
            begin
              StringStatement;
              Statement;
            end;
        else
          StringConst;
        end;
//        if Lexer.TokenID = ptPoint then
//        begin
//          NextToken;
//          VariableReference;
//        end;
      end;
  else
    begin
      SynError(InvalidCharString);
    end;
  end;
end;

procedure TmwSimplePasPar.IncludeFile;
begin
  while TokenID <> ptNull do
    case TokenID of
      ptClass:
        begin
          ProcedureDeclarationSection;
        end;
      ptConst:
        begin
          ConstSection;
        end;
      ptConstructor:
        begin
          ProcedureDeclarationSection;
        end;
      ptDestructor:
        begin
          ProcedureDeclarationSection;
        end;
      ptExports:
        begin
          ExportsClause;
        end;
      ptFunction:
        begin
          ProcedureDeclarationSection;
        end;
      ptIdentifier:
        begin
          Statements;
        end;
      ptLabel:
        begin
          LabelDeclarationSection;
        end;
      ptProcedure:
        begin
          ProcedureDeclarationSection;
        end;
      ptResourceString:
        begin
          ConstSection;
        end;
      ptType:
        begin
          TypeSection;
        end;
      ptThreadVar:
        begin
          VarSection;
        end;
      ptVar:
        begin
          VarSection;
        end;
    else
      begin
        NextToken;
      end;
    end;
end;

procedure TmwSimplePasPar.SkipSpace;
begin
  Expected(ptSpace);
  while TokenID in [ptSpace] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.SkipCRLFco;
begin
  Expected(ptCRLFCo);
  while TokenID in [ptCRLFCo] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.SkipCRLF;
begin
  Expected(ptCRLF);
  while TokenID in [ptCRLF] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.ClassClass;
begin
  Expected(ptClass);
end;

procedure TmwSimplePasPar.ClassConstraint;
begin
  Expected(ptClass);
end;

procedure TmwSimplePasPar.PropertyDefault;
begin
  ExpectedEx(ptDefault);
end;

procedure TmwSimplePasPar.DispIDSpecifier;
begin
  ExpectedEx(ptDispid);
  ConstantExpression;
end;

procedure TmwSimplePasPar.IndexOp;
begin
  Expected(ptSquareOpen);
  Expression;
  while TokenID = ptComma do
  begin
    NextToken;
    Expression;
  end;
  Expected(ptSquareClose);
end;

procedure TmwSimplePasPar.IndexSpecifier;
begin
  ExpectedEx(ptIndex);
  ConstantExpression;
end;

procedure TmwSimplePasPar.ClassTypeEnd;
begin
  case ExID of
    ptExperimental: NextToken;
    ptDeprecated: DirectiveDeprecated;
  end;
end;

procedure TmwSimplePasPar.ObjectTypeEnd;
begin
end;

procedure TmwSimplePasPar.DirectiveDeprecated;
begin
  ExpectedEx(ptDeprecated);
  if TokenID = ptStringConst then
    NextToken;
end;

procedure TmwSimplePasPar.DirectiveInline;
begin
  Expected(ptInline);
end;

procedure TmwSimplePasPar.DirectiveLibrary;
begin
  Expected(ptLibrary);
end;

procedure TmwSimplePasPar.DirectivePlatform;
begin
  ExpectedEx(ptPlatform);
end;

procedure TmwSimplePasPar.EnumeratedTypeItem;
begin
  QualifiedIdentifier;
  if TokenID = ptEqual then
  begin
    Expected(ptEqual);
    ConstantExpression;
  end;
end;

procedure TmwSimplePasPar.Identifier;
begin
  NextToken;
end;

procedure TmwSimplePasPar.DirectiveLocal;
begin
  ExpectedEx(ptLocal);
end;

procedure TmwSimplePasPar.DirectiveVarargs;
begin
  ExpectedEx(ptVarargs);
end;

procedure TmwSimplePasPar.AncestorId;
begin
  TypeId;
end;

procedure TmwSimplePasPar.AncestorIdList;
begin
  AncestorId;
  while(TokenID = ptComma) do
    begin
      NextToken;
      AncestorId;
    end;
end;

procedure TmwSimplePasPar.AnonymousMethod;
begin
  case TokenID of
    ptFunction:
      begin
        NextToken;
        if TokenID = ptRoundOpen then
          FormalParameterList;
        Expected(ptColon);
        ReturnType;
      end;
    ptProcedure:
      begin
        NextToken;
        if TokenId = ptRoundOpen then
          FormalParameterList;
      end;
  end;
  Block;
end;

procedure TmwSimplePasPar.AnonymousMethodType;
begin
  ExpectedEx(ptReference);
  Expected(ptTo);
  case TokenID of
    ptProcedure:
      begin
        NextToken;
        if TokenID = ptRoundOpen then
          FormalParameterList;
      end;
    ptFunction:
      begin
        NextToken;
        if TokenID = ptRoundOpen then
          FormalParameterList;
        Expected(ptColon);
        ReturnType;
      end;
  end;
end;

procedure TmwSimplePasPar.AddDefine(const ADefine: string);
begin
  FLexer.AddDefine(ADefine);
end;

procedure TmwSimplePasPar.RemoveDefine(const ADefine: string);
begin
  FLexer.RemoveDefine(ADefine);
end;

function TmwSimplePasPar.IsDefined(const ADefine: string): Boolean;
begin
  Result := FLexer.IsDefined(ADefine);
end;

procedure TmwSimplePasPar.IsNotOp;
begin
  Expected(ptIs);
  Expected(ptNot);
end;

procedure TmwSimplePasPar.ExportsNameId;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.ExportsName;
begin
  ExportsNameId;
  while FLexer.TokenID = ptPoint do
  begin
    NextToken;
    ExportsNameId;
  end;
end;

procedure TmwSimplePasPar.ImplementsSpecifier;
begin
  ExpectedEx(ptImplements);

  TypeId;
  while (TokenID = ptComma) do
  begin
    NextToken;
    TypeId;
  end;
end;

procedure TmwSimplePasPar.AttributeArgumentName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.CaseLabelList;
begin
  CaseLabel;
  while TokenID = ptComma do
  begin
    NextToken;
    CaseLabel;
  end;
end;

procedure TmwSimplePasPar.ArrayBounds;
begin
  if TokenID = ptSquareOpen then
  begin
    NextToken;
    ArrayDimension;
    while TokenID = ptComma do
    begin
      NextToken;
      ArrayDimension;
    end;
    Expected(ptSquareClose);
  end;
end;

procedure TmwSimplePasPar.DeclarationSections;
begin
  while TokenID in [ptClass, ptConst, ptConstructor, ptDestructor, ptExports, ptFunction, ptLabel, ptProcedure, ptResourceString, ptThreadVar, ptType, ptVar, ptSquareOpen] do
  begin
    DeclarationSection;
  end;
end;

procedure TmwSimplePasPar.ProceduralDirectiveOf;
begin
  NextToken;
  Expected(ptObject);
end;

procedure TmwSimplePasPar.TypeDirective;
begin
  while GenID in [ptDeprecated, ptLibrary, ptPlatform, ptExperimental] do
    case GenID of
      ptDeprecated:   DirectiveDeprecated;
      ptLibrary:      DirectiveLibrary;
      ptPlatform:     DirectivePlatform;
      ptExperimental: NextToken;
    end;
end;

procedure TmwSimplePasPar.InheritedVariableReference;
begin
  Expected(ptInherited);
  if TokenID = ptIdentifier then
    VariableReference;
end;

procedure TmwSimplePasPar.ClearDefines;
begin
  FLexer.ClearDefines;
end;

procedure TmwSimplePasPar.InitAhead;
begin
  if AheadParse = nil then
    AheadParse := TmwSimplePasPar.Create;
  AheadParse.Lexer.InitFrom(Lexer);
end;

procedure TmwSimplePasPar.InitDefinesDefinedByCompiler;
begin
  FLexer.InitDefinesDefinedByCompiler;
end;

procedure TmwSimplePasPar.GlobalAttributes;
begin
  GlobalAttributeSections;
end;

procedure TmwSimplePasPar.GlobalAttributeSections;
begin
  while TokenID = ptSquareOpen do
    GlobalAttributeSection;
end;

procedure TmwSimplePasPar.GlobalAttributeSection;
begin
  Expected(ptSquareOpen);
  GlobalAttributeTargetSpecifier;
  AttributeList;
  while TokenID = ptComma do
  begin
    Expected(ptComma);
    GlobalAttributeTargetSpecifier;
    AttributeList;
  end;
  Expected(ptSquareClose);
end;

procedure TmwSimplePasPar.GlobalAttributeTargetSpecifier;
begin
  GlobalAttributeTarget;
  Expected(ptColon);
end;

procedure TmwSimplePasPar.GlobalAttributeTarget;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.Attributes;
begin
  AttributeSections;
end;

procedure TmwSimplePasPar.AttributeSections;
begin
  while TokenID = ptSquareOpen do
    AttributeSection;
end;

procedure TmwSimplePasPar.AttributeSection;
begin
  Expected(ptSquareOpen);
  Lexer.InitAhead;
  if Lexer.AheadTokenID = ptColon then
    AttributeTargetSpecifier;
  AttributeList;
  while TokenID = ptComma do
  begin
    Lexer.InitAhead;
    if Lexer.AheadTokenID = ptColon then
      AttributeTargetSpecifier;
    AttributeList;
  end;
  Expected(ptSquareClose);
end;

procedure TmwSimplePasPar.AttributeTargetSpecifier;
begin
  AttributeTarget;
  Expected(ptColon);
end;

procedure TmwSimplePasPar.AttributeTarget;
begin
  case TokenID of
    ptProperty:
      Expected(ptProperty);
    ptType:
      Expected(ptType);
    else
      Expected(ptIdentifier);
  end;
end;

procedure TmwSimplePasPar.AttributeList;
begin
  Attribute;
  while TokenID = ptComma do
  begin
    Expected(ptComma);
    AttributeList;
  end;
end;

procedure TmwSimplePasPar.Attribute;
begin
  AttributeName;
  if TokenID = ptRoundOpen then
    AttributeArguments;
end;

procedure TmwSimplePasPar.AttributeName;
begin
  case TokenID of
    ptIn, ptOut, ptConst, ptVar, ptUnsafe:
      NextToken;
  else
    begin
      Expected(ptIdentifier);
      while TokenID = ptPoint do
      begin
        NextToken;
        Expected(ptIdentifier);
      end;
    end;
  end;
end;

procedure TmwSimplePasPar.AttributeArguments;
begin
  Expected(ptRoundOpen);
  if TokenID <> ptRoundClose then
  begin
    Lexer.InitAhead;
    if Lexer.AheadTokenID = ptEqual then
      NamedArgumentList
    else
      PositionalArgumentList;
    if Lexer.TokenID = ptEqual then
      NamedArgumentList;
  end;
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.PositionalArgumentList;
begin
  PositionalArgument;
  while TokenID = ptComma do
  begin
    Expected(ptComma);
    PositionalArgument;
  end;
end;

procedure TmwSimplePasPar.PositionalArgument;
begin
  AttributeArgumentExpression;
end;

procedure TmwSimplePasPar.NamedArgumentList;
begin
  NamedArgument;
  while TokenID = ptComma do
  begin
    Expected(ptComma);
    NamedArgument;
  end;
end;

procedure TmwSimplePasPar.NamedArgument;
begin
  AttributeArgumentName;
  Expected(ptEqual);
  AttributeArgumentExpression;
end;

procedure TmwSimplePasPar.AttributeArgumentExpression;
begin
  Expression;
end;

procedure TmwSimplePasPar.CustomAttribute;
begin
  //TODO: Global vs. Local attributes
  AttributeSections;
end;

end.