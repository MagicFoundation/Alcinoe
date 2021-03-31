{*******************************************************************************
Author(s):
Jedi Project - JCL
Barry Kelly
Matthias Thoma (mthoma)
Petr Vones (pvones)
Robert Marquardt (marquardt)
Robert Rossmair (rrossmair)

Description:
This unit contains expression evaluators, each tailored for different usage
patterns. It also contains the component objects, so that a customized
expression evaluator can be assembled relatively easily.

Note :
operator priority (as implemented in this unit)
all binary operators are associated from left to right
all unary operators are associated from right to left

(highest) not bnot(bitwise) +(unary) -(unary)            (level 3)
* / div mod and band(bitwise) shl shr in                 (level 2)
+(binary) -(binary) or xor bor(bitwise) bxor(bitwise)    (level 1)
(lowest)  < <= > >= cmp = <>                             (level 0)

details on cmp operator:
"1.5 cmp 2.0" returns -1.0 because 1.5 < 2.0
"1.5 cmp 1.5" returns 0.0 because 1.5 = 1.5
"1.5 cmp 0.0" returns 1.0 because 1.5 > 0.0
*******************************************************************************}

unit ALExprEval;

interface

{$IF CompilerVersion > 34} // sydney
  {$MESSAGE WARN 'Check if https://github.com/project-jedi/jcl.git jcl\source\common\JclExprEval.pas was not updated from references\jcl\source\common\JclExprEval.pas and adjust the IFDEF'}
{$IFEND}

uses
  System.SysUtils,
  System.Classes,
  ALStringList;

type
  EALExprEvalError = class(Exception);

type

  ALFloat = Extended;

  PALFloat = ^ALFloat;

  TALFloat = ALFloat;

  TALFloat32 = Single;
  PALFloat32 = ^TALFloat32;

  TALFloat64 = Double;
  PALFloat64 = ^TALFloat64;

  TALFloat80 = Extended;
  PALFloat80 = ^TALFloat80;

  TALFloatFunc = function: TALFloat;
  TALFloat32Func = function: TALFloat32;
  TALFloat64Func = function: TALFloat64;
  TALFloat80Func = function: TALFloat80;

  TALUnaryFunc = function(X: TALFloat): TALFloat;
  TALUnary32Func = function(X: TALFloat32): TALFloat32;
  TALUnary64Func = function(X: TALFloat64): TALFloat64;
  TALUnary80Func = function(X: TALFloat80): TALFloat80;

  TALBinaryFunc = function(X, Y: TALFloat): TALFloat;
  TALBinary32Func = function(X, Y: TALFloat32): TALFloat32;
  TALBinary64Func = function(X, Y: TALFloat64): TALFloat64;
  TALBinary80Func = function(X, Y: TALFloat80): TALFloat80;

  TALTernaryFunc = function(X, Y, Z: TALFloat): TALFloat;
  TALTernary32Func = function(X, Y, Z: TALFloat32): TALFloat32;
  TALTernary64Func = function(X, Y, Z: TALFloat64): TALFloat64;
  TALTernary80Func = function(X, Y, Z: TALFloat80): TALFloat80;

type
  { Forward Declarations }
  TALExprLexer = class;
  TALExprCompileParser = class;
  TALExprEvalParser = class;
  TALExprSym = class;
  TALExprNode = class;
  TALExprNodeFactory = class;

  TALExprContext = class(TObject)
  public
    function Find(const AName: AnsiString): TALExprSym; virtual; abstract;
  end;

  TALExprHashContext = class(TALExprContext)
  private
    FHashMap: TALAVLStringList;
  public
    constructor Create(ACaseSensitive: Boolean = False);
    destructor Destroy; override;
    procedure Add(ASymbol: TALExprSym);
    procedure Remove(const AName: AnsiString);
    function Find(const AName: AnsiString): TALExprSym; override;
  end;

  TALExprSetContext = class(TALExprContext)
  private
    FList: TList;
    FOwnsContexts: Boolean;
    function GetContexts(AIndex: Integer): TALExprContext;
    function GetCount: Integer;
  public
    constructor Create(AOwnsContexts: Boolean);
    destructor Destroy; override;
    procedure Add(AContext: TALExprContext);
    procedure Remove(AContext: TALExprContext);
    procedure Delete(AIndex: Integer);
    function Extract(AContext: TALExprContext): TALExprContext;
    property Count: Integer read GetCount;
    property Contexts[AIndex: Integer]: TALExprContext read GetContexts;
    property InternalList: TList read FList;
    function Find(const AName: AnsiString): TALExprSym; override;
  end;

  TALExprSym = class(TObject)
  private
    FIdent: AnsiString;
    FLexer: TALExprLexer;
    FEvalParser: TALExprEvalParser;
    FCompileParser: TALExprCompileParser;
    FNodeFactory: TALExprNodeFactory;
  public
    constructor Create(const AIdent: AnsiString);
    function Evaluate: TALFloat; virtual; abstract;
    function Compile: TALExprNode; virtual; abstract;
    property Ident: AnsiString read FIdent;
    property Lexer: TALExprLexer read FLexer write FLexer;
    property CompileParser: TALExprCompileParser read FCompileParser
      write FCompileParser;
    property EvalParser: TALExprEvalParser read FEvalParser write FEvalParser;
    property NodeFactory: TALExprNodeFactory read FNodeFactory write FNodeFactory;
  end;

  TALExprToken = (
    // specials
    etEof,
    etNumber,
    etIdentifier,

    // user extension tokens
    etUser0, etUser1, etUser2, etUser3, etUser4, etUser5, etUser6, etUser7,
    etUser8, etUser9, etUser10, etUser11, etUser12, etUser13, etUser14, etUser15,
    etUser16, etUser17, etUser18, etUser19, etUser20, etUser21, etUser22, etUser23,
    etUser24, etUser25, etUser26, etUser27, etUser28, etUser29, etUser30, etUser31,

    // compound tokens
    etNotEqual, // <>
    etLessEqual, // <=
    etGreaterEqual, // >=

    // ASCII normal & ordinals

    etBang, // '!' #$21 33
    etDoubleQuote, // '"' #$22 34
    etHash, // '#' #$23 35
    etDollar, // '$' #$24 36
    etPercent, // '%' #$25 37
    etAmpersand, // '&' #$26 38
    etSingleQuote, // '''' #$27 39
    etLParen, // '(' #$28 40
    etRParen, // ')' #$29 41
    etAsterisk, // '*' #$2A 42
    etPlus, // '+' #$2B 43
    etComma, // ',' #$2C 44
    etMinus, // '-' #$2D 45
    etDot, // '.' #$2E 46
    etForwardSlash, // '/' #$2F 47

    // 48..57 - numbers...

    etColon, // ':' #$3A 58
    etSemiColon, // ';' #$3B 59
    etLessThan, // '<' #$3C 60
    etEqualTo, // '=' #$3D 61
    etGreaterThan, // '>' #$3E 62
    etQuestion, // '?' #$3F 63
    etAt, // '@' #$40 64

    // 65..90 - capital letters...

    etLBracket, // '[' #$5B 91
    etBackSlash, // '\' #$5C 92
    etRBracket, // ']' #$5D 93
    etArrow, // '^' #$5E 94
    // 95 - underscore
    etBackTick, // '`' #$60 96

    // 97..122 - small letters...

    etLBrace, // '{' #$7B 123
    etPipe, // '|' #$7C 124
    etRBrace, // '}' #$7D 125
    etTilde, // '~' #$7E 126
    et127, // '' #$7F 127
    etEuro, // '€' #$80 128
    et129, // '' #$81 129
    et130, // '‚' #$82 130
    et131, // 'ƒ' #$83 131
    et132, // '„' #$84 132
    et133, // '…' #$85 133
    et134, // '†' #$86 134
    et135, // '‡' #$87 135
    et136, // 'ˆ' #$88 136
    et137, // '‰' #$89 137
    et138, // 'Š' #$8A 138
    et139, // '‹' #$8B 139
    et140, // 'Œ' #$8C 140
    et141, // '' #$8D 141
    et142, // 'Ž' #$8E 142
    et143, // '' #$8F 143
    et144, // '' #$90 144
    et145, // '‘' #$91 145
    et146, // '’' #$92 146
    et147, // '“' #$93 147
    et148, // '”' #$94 148
    et149, // '•' #$95 149
    et150, // '–' #$96 150
    et151, // '—' #$97 151
    et152, // '˜' #$98 152
    et153, // '™' #$99 153
    et154, // 'š' #$9A 154
    et155, // '›' #$9B 155
    et156, // 'œ' #$9C 156
    et157, // '' #$9D 157
    et158, // 'ž' #$9E 158
    et159, // 'Ÿ' #$9F 159
    et160, // ' ' #$A0 160
    et161, // '¡' #$A1 161
    et162, // '¢' #$A2 162
    et163, // '£' #$A3 163
    et164, // '¤' #$A4 164
    et165, // '¥' #$A5 165
    et166, // '¦' #$A6 166
    et167, // '§' #$A7 167
    et168, // '¨' #$A8 168
    et169, // '©' #$A9 169
    et170, // 'ª' #$AA 170
    et171, // '«' #$AB 171
    et172, // '¬' #$AC 172
    et173, // '­' #$AD 173
    et174, // '®' #$AE 174
    et175, // '¯' #$AF 175
    et176, // '°' #$B0 176
    et177, // '±' #$B1 177
    et178, // '²' #$B2 178
    et179, // '³' #$B3 179
    et180, // '´' #$B4 180
    et181, // 'µ' #$B5 181
    et182, // '¶' #$B6 182
    et183, // '·' #$B7 183
    et184, // '¸' #$B8 184
    et185, // '¹' #$B9 185
    et186, // 'º' #$BA 186
    et187, // '»' #$BB 187
    et188, // '¼' #$BC 188
    et189, // '½' #$BD 189
    et190, // '¾' #$BE 190
    et191, // '¿' #$BF 191
    et192, // 'À' #$C0 192
    et193, // 'Á' #$C1 193
    et194, // 'Â' #$C2 194
    et195, // 'Ã' #$C3 195
    et196, // 'Ä' #$C4 196
    et197, // 'Å' #$C5 197
    et198, // 'Æ' #$C6 198
    et199, // 'Ç' #$C7 199
    et200, // 'È' #$C8 200
    et201, // 'É' #$C9 201
    et202, // 'Ê' #$CA 202
    et203, // 'Ë' #$CB 203
    et204, // 'Ì' #$CC 204
    et205, // 'Í' #$CD 205
    et206, // 'Î' #$CE 206
    et207, // 'Ï' #$CF 207
    et208, // 'Ð' #$D0 208
    et209, // 'Ñ' #$D1 209
    et210, // 'Ò' #$D2 210
    et211, // 'Ó' #$D3 211
    et212, // 'Ô' #$D4 212
    et213, // 'Õ' #$D5 213
    et214, // 'Ö' #$D6 214
    et215, // '×' #$D7 215
    et216, // 'Ø' #$D8 216
    et217, // 'Ù' #$D9 217
    et218, // 'Ú' #$DA 218
    et219, // 'Û' #$DB 219
    et220, // 'Ü' #$DC 220
    et221, // 'Ý' #$DD 221
    et222, // 'Þ' #$DE 222
    et223, // 'ß' #$DF 223
    et224, // 'à' #$E0 224
    et225, // 'á' #$E1 225
    et226, // 'â' #$E2 226
    et227, // 'ã' #$E3 227
    et228, // 'ä' #$E4 228
    et229, // 'å' #$E5 229
    et230, // 'æ' #$E6 230
    et231, // 'ç' #$E7 231
    et232, // 'è' #$E8 232
    et233, // 'é' #$E9 233
    et234, // 'ê' #$EA 234
    et235, // 'ë' #$EB 235
    et236, // 'ì' #$EC 236
    et237, // 'í' #$ED 237
    et238, // 'î' #$EE 238
    et239, // 'ï' #$EF 239
    et240, // 'ð' #$F0 240
    et241, // 'ñ' #$F1 241
    et242, // 'ò' #$F2 242
    et243, // 'ó' #$F3 243
    et244, // 'ô' #$F4 244
    et245, // 'õ' #$F5 245
    et246, // 'ö' #$F6 246
    et247, // '÷' #$F7 247
    et248, // 'ø' #$F8 248
    et249, // 'ù' #$F9 249
    et250, // 'ú' #$FA 250
    et251, // 'û' #$FB 251
    et252, // 'ü' #$FC 252
    et253, // 'ý' #$FD 253
    et254, // 'þ' #$FE 254
    et255, // 'ÿ' #$FF 255
    etInvalid // invalid token type
  );

  TALExprLexer = class(TObject)
  protected
    FCurrTok: TALExprToken;
    FTokenAsNumber: TALFloat;
    FTokenAsString: AnsiString;
  public
    constructor Create;
    procedure NextTok(const EvalTok: Boolean = True); virtual; abstract;
    procedure Reset; virtual;
    property TokenAsString: AnsiString read FTokenAsString;
    property TokenAsNumber: TALFloat read FTokenAsNumber;
    property CurrTok: TALExprToken read FCurrTok;
  end;

  TALExprNode = class(TObject)
  private
    FDepList: TList;
    function GetDepCount: Integer;
    function GetDeps(AIndex: Integer): TALExprNode;
  public
    constructor Create(const ADepList: array of TALExprNode);
    destructor Destroy; override;
    procedure AddDep(ADep: TALExprNode);
    property DepCount: Integer read GetDepCount;
    property Deps[AIndex: Integer]: TALExprNode read GetDeps; default;
    property DepList: TList read FDepList;
  end;

  TALExprNodeFactory = class(TObject)
  public
    function LoadVar32(ALoc: PALFloat32): TALExprNode; virtual; abstract;
    function LoadVar64(ALoc: PALFloat64): TALExprNode; virtual; abstract;
    function LoadVar80(ALoc: PALFloat80): TALExprNode; virtual; abstract;

    function LoadConst32(AValue: TALFloat32): TALExprNode; virtual; abstract;
    function LoadConst64(AValue: TALFloat64): TALExprNode; virtual; abstract;
    function LoadConst80(AValue: TALFloat80): TALExprNode; virtual; abstract;

    function CallFloatFunc(AFunc: TALFloatFunc): TALExprNode; virtual; abstract;
    function CallFloat32Func(AFunc: TALFloat32Func): TALExprNode; virtual; abstract;
    function CallFloat64Func(AFunc: TALFloat64Func): TALExprNode; virtual; abstract;
    function CallFloat80Func(AFunc: TALFloat80Func): TALExprNode; virtual; abstract;
    function CallUnaryFunc(AFunc: TALUnaryFunc; X: TALExprNode): TALExprNode; virtual; abstract;
    function CallUnary32Func(AFunc: TALUnary32Func; X: TALExprNode): TALExprNode; virtual; abstract;
    function CallUnary64Func(AFunc: TALUnary64Func; X: TALExprNode): TALExprNode; virtual; abstract;
    function CallUnary80Func(AFunc: TALUnary80Func; X: TALExprNode): TALExprNode; virtual; abstract;
    function CallBinaryFunc(AFunc: TALBinaryFunc; X, Y: TALExprNode): TALExprNode; virtual; abstract;
    function CallBinary32Func(AFunc: TALBinary32Func; X, Y: TALExprNode): TALExprNode; virtual; abstract;
    function CallBinary64Func(AFunc: TALBinary64Func; X, Y: TALExprNode): TALExprNode; virtual; abstract;
    function CallBinary80Func(AFunc: TALBinary80Func; X, Y: TALExprNode): TALExprNode; virtual; abstract;
    function CallTernaryFunc(AFunc: TALTernaryFunc; X, Y, Z: TALExprNode): TALExprNode; virtual; abstract;
    function CallTernary32Func(AFunc: TALTernary32Func; X, Y, Z: TALExprNode): TALExprNode; virtual; abstract;
    function CallTernary64Func(AFunc: TALTernary64Func; X, Y, Z: TALExprNode): TALExprNode; virtual; abstract;
    function CallTernary80Func(AFunc: TALTernary80Func; X, Y, Z: TALExprNode): TALExprNode; virtual; abstract;

    function Add(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;
    function Subtract(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;
    function Multiply(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;
    function Divide(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;
    function IntegerDivide(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;
    function Modulo(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;
    function Negate(AValue: TALExprNode): TALExprNode; virtual; abstract;

    function Compare(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;
    function CompareEqual(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;
    function CompareNotEqual(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;
    function CompareLess(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;
    function CompareLessEqual(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;
    function CompareGreater(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;
    function CompareGreaterEqual(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;

    function LogicalAnd(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;
    function LogicalOr(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;
    function LogicalXor(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;
    function LogicalNot(AValue: TALExprNode): TALExprNode; virtual; abstract;
    function BitwiseAnd(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;
    function BitwiseOr(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;
    function BitwiseXor(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;
    function BitwiseNot(AValue: TALExprNode): TALExprNode; virtual; abstract;
    function ShiftLeft(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;
    function ShiftRight(ALeft, ARight: TALExprNode): TALExprNode; virtual; abstract;

    function LoadVar(ALoc: PALFloat32): TALExprNode; overload;
    function LoadVar(ALoc: PALFloat64): TALExprNode; overload;
    function LoadVar(ALoc: PALFloat80): TALExprNode; overload;
    function LoadConst(AValue: TALFloat32): TALExprNode; overload;
    function LoadConst(AValue: TALFloat64): TALExprNode; overload;
    function LoadConst(AValue: TALFloat80): TALExprNode; overload;
  end;

  TALExprCompileParser = class(TObject)
  private
    FContext: TALExprContext;
    FLexer: TALExprLexer;
    FNodeFactory: TALExprNodeFactory;
  protected
    function CompileExprLevel0(ASkip: Boolean): TALExprNode; virtual;
    function CompileExprLevel1(ASkip: Boolean): TALExprNode; virtual;
    function CompileExprLevel2(ASkip: Boolean): TALExprNode; virtual;
    function CompileExprLevel3(ASkip: Boolean): TALExprNode; virtual;
    function CompileFactor: TALExprNode; virtual;
    function CompileIdentFactor: TALExprNode; virtual;
  public
    constructor Create(ALexer: TALExprLexer; ANodeFactory: TALExprNodeFactory);
    function Compile: TALExprNode; virtual;
    property Lexer: TALExprLexer read FLexer;
    property NodeFactory: TALExprNodeFactory read FNodeFactory;
    property Context: TALExprContext read FContext write FContext;
  end;

  TALExprEvalParser = class(TObject)
  private
    FContext: TALExprContext;
    FLexer: TALExprLexer;
  protected
    function EvalExprLevel0(ASkip: Boolean): TALFloat; virtual;
    function EvalExprLevel1(ASkip: Boolean): TALFloat; virtual;
    function EvalExprLevel2(ASkip: Boolean): TALFloat; virtual;
    function EvalExprLevel3(ASkip: Boolean): TALFloat; virtual;
    function EvalFactor: TALFloat; virtual;
    function EvalIdentFactor: TALFloat; virtual;
  public
    constructor Create(ALexer: TALExprLexer);
    function Evaluate: TALFloat; virtual;

    property Lexer: TALExprLexer read FLexer;
    property Context: TALExprContext read FContext write FContext;
  end;

{ some concrete class descendants follow... }

  TALExprSimpleLexer = class(TALExprLexer)
  protected
    FCurrPos: PAnsiChar;
    FBuf: AnsiString;
    procedure SetBuf(const ABuf: AnsiString);
  public
    constructor Create(const ABuf: AnsiString);

    procedure NextTok(const EvalTok: Boolean = True); override;
    procedure Reset; override;

    property Buf: AnsiString read FBuf write SetBuf;
  end;

  TALExprVirtMachOp = class(TObject)
  private
    function GetOutputLoc: PALFloat;
  protected
    FOutput: TALFloat;
  public
    procedure Execute; virtual; abstract;
    property OutputLoc: PALFloat read GetOutputLoc;
  end;

  TALExprVirtMach = class(TObject)
  private
    FCodeList: TList;
    FConstList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AOp: TALExprVirtMachOp);
    procedure AddConst(AOp: TALExprVirtMachOp);
    procedure Clear;
    function Execute: TALFloat;
  end;

  TALExprVirtMachNodeFactory = class(TALExprNodeFactory)
  private
    FNodeList: TList;
    function AddNode(ANode: TALExprNode): TALExprNode;
    procedure DoClean(AVirtMach: TALExprVirtMach);
    procedure DoConsts(AVirtMach: TALExprVirtMach);
    procedure DoCode(AVirtMach: TALExprVirtMach);
  public
    constructor Create;
    destructor Destroy; override;

    procedure GenCode(AVirtMach: TALExprVirtMach);

    function LoadVar32(ALoc: PALFloat32): TALExprNode; override;
    function LoadVar64(ALoc: PALFloat64): TALExprNode; override;
    function LoadVar80(ALoc: PALFloat80): TALExprNode; override;
    function LoadConst32(AValue: TALFloat32): TALExprNode; override;
    function LoadConst64(AValue: TALFloat64): TALExprNode; override;
    function LoadConst80(AValue: TALFloat80): TALExprNode; override;

    function CallFloatFunc(AFunc: TALFloatFunc): TALExprNode; override;
    function CallFloat32Func(AFunc: TALFloat32Func): TALExprNode; override;
    function CallFloat64Func(AFunc: TALFloat64Func): TALExprNode; override;
    function CallFloat80Func(AFunc: TALFloat80Func): TALExprNode; override;
    function CallUnaryFunc(AFunc: TALUnaryFunc; X: TALExprNode): TALExprNode; override;
    function CallUnary32Func(AFunc: TALUnary32Func; X: TALExprNode): TALExprNode; override;
    function CallUnary64Func(AFunc: TALUnary64Func; X: TALExprNode): TALExprNode; override;
    function CallUnary80Func(AFunc: TALUnary80Func; X: TALExprNode): TALExprNode; override;
    function CallBinaryFunc(AFunc: TALBinaryFunc; X, Y: TALExprNode): TALExprNode; override;
    function CallBinary32Func(AFunc: TALBinary32Func; X, Y: TALExprNode): TALExprNode; override;
    function CallBinary64Func(AFunc: TALBinary64Func; X, Y: TALExprNode): TALExprNode; override;
    function CallBinary80Func(AFunc: TALBinary80Func; X, Y: TALExprNode): TALExprNode; override;
    function CallTernaryFunc(AFunc: TALTernaryFunc; X, Y, Z: TALExprNode): TALExprNode; override;
    function CallTernary32Func(AFunc: TALTernary32Func; X, Y, Z: TALExprNode): TALExprNode; override;
    function CallTernary64Func(AFunc: TALTernary64Func; X, Y, Z: TALExprNode): TALExprNode; override;
    function CallTernary80Func(AFunc: TALTernary80Func; X, Y, Z: TALExprNode): TALExprNode; override;

    function Add(ALeft, ARight: TALExprNode): TALExprNode; override;
    function Subtract(ALeft, ARight: TALExprNode): TALExprNode; override;
    function Multiply(ALeft, ARight: TALExprNode): TALExprNode; override;
    function Divide(ALeft, ARight: TALExprNode): TALExprNode; override;
    function IntegerDivide(ALeft, ARight: TALExprNode): TALExprNode; override;
    function Modulo(ALeft, ARight: TALExprNode): TALExprNode; override;
    function Negate(AValue: TALExprNode): TALExprNode; override;

    function Compare(ALeft, ARight: TALExprNode): TALExprNode; override;
    function CompareEqual(ALeft, ARight: TALExprNode): TALExprNode; override;
    function CompareNotEqual(ALeft, ARight: TALExprNode): TALExprNode; override;
    function CompareLess(ALeft, ARight: TALExprNode): TALExprNode; override;
    function CompareLessEqual(ALeft, ARight: TALExprNode): TALExprNode; override;
    function CompareGreater(ALeft, ARight: TALExprNode): TALExprNode; override;
    function CompareGreaterEqual(ALeft, ARight: TALExprNode): TALExprNode; override;

    function LogicalAnd(ALeft, ARight: TALExprNode): TALExprNode; override;
    function LogicalOr(ALeft, ARight: TALExprNode): TALExprNode; override;
    function LogicalXor(ALeft, ARight: TALExprNode): TALExprNode; override;
    function LogicalNot(AValue: TALExprNode): TALExprNode; override;
    function BitwiseAnd(ALeft, ARight: TALExprNode): TALExprNode; override;
    function BitwiseOr(ALeft, ARight: TALExprNode): TALExprNode; override;
    function BitwiseXor(ALeft, ARight: TALExprNode): TALExprNode; override;
    function BitwiseNot(AValue: TALExprNode): TALExprNode; override;
    function ShiftLeft(ALeft, ARight: TALExprNode): TALExprNode; override;
    function ShiftRight(ALeft, ARight: TALExprNode): TALExprNode; override;
  end;

  { some concrete symbols }

  TALExprConstSym = class(TALExprSym)
  private
    FValue: TALFloat;
  public
    constructor Create(const AIdent: AnsiString; AValue: TALFloat);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprConst32Sym = class(TALExprSym)
  private
    FValue: TALFloat32;
  public
    constructor Create(const AIdent: AnsiString; AValue: TALFloat32);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprConst64Sym = class(TALExprSym)
  private
    FValue: TALFloat64;
  public
    constructor Create(const AIdent: AnsiString; AValue: TALFloat64);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprConst80Sym = class(TALExprSym)
  private
    FValue: TALFloat80;
  public
    constructor Create(const AIdent: AnsiString; AValue: TALFloat80);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprVar32Sym = class(TALExprSym)
  private
    FLoc: PALFloat32;
  public
    constructor Create(const AIdent: AnsiString; ALoc: PALFloat32);

    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprVar64Sym = class(TALExprSym)
  private
    FLoc: PALFloat64;
  public
    constructor Create(const AIdent: AnsiString; ALoc: PALFloat64);

    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprVar80Sym = class(TALExprSym)
  private
    FLoc: PALFloat80;
  public
    constructor Create(const AIdent: AnsiString; ALoc: PALFloat80);

    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprAbstractFuncSym = class(TALExprSym)
  protected
    function EvalFirstArg: TALFloat;
    function EvalNextArg: TALFloat;
    function CompileFirstArg: TALExprNode;
    function CompileNextArg: TALExprNode;
    procedure EndArgs;
  end;

  TALExprFuncSym = class(TALExprAbstractFuncSym)
  private
    FFunc: TALFloatFunc;
  public
    constructor Create(const AIdent: AnsiString; AFunc: TALFloatFunc);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprFloat32FuncSym = class(TALExprAbstractFuncSym)
  private
    FFunc: TALFloat32Func;
  public
    constructor Create(const AIdent: AnsiString; AFunc: TALFloat32Func);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprFloat64FuncSym = class(TALExprAbstractFuncSym)
  private
    FFunc: TALFloat64Func;
  public
    constructor Create(const AIdent: AnsiString; AFunc: TALFloat64Func);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprFloat80FuncSym = class(TALExprAbstractFuncSym)
  private
    FFunc: TALFloat80Func;
  public
    constructor Create(const AIdent: AnsiString; AFunc: TALFloat80Func);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprUnaryFuncSym = class(TALExprAbstractFuncSym)
  private
    FFunc: TALUnaryFunc;
  public
    constructor Create(const AIdent: AnsiString; AFunc: TALUnaryFunc);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprUnary32FuncSym = class(TALExprAbstractFuncSym)
  private
    FFunc: TALUnary32Func;
  public
    constructor Create(const AIdent: AnsiString; AFunc: TALUnary32Func);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprUnary64FuncSym = class(TALExprAbstractFuncSym)
  private
    FFunc: TALUnary64Func;
  public
    constructor Create(const AIdent: AnsiString; AFunc: TALUnary64Func);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprUnary80FuncSym = class(TALExprAbstractFuncSym)
  private
    FFunc: TALUnary80Func;
  public
    constructor Create(const AIdent: AnsiString; AFunc: TALUnary80Func);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprBinaryFuncSym = class(TALExprAbstractFuncSym)
  private
    FFunc: TALBinaryFunc;
  public
    constructor Create(const AIdent: AnsiString; AFunc: TALBinaryFunc);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprBinary32FuncSym = class(TALExprAbstractFuncSym)
  private
    FFunc: TALBinary32Func;
  public
    constructor Create(const AIdent: AnsiString; AFunc: TALBinary32Func);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprBinary64FuncSym = class(TALExprAbstractFuncSym)
  private
    FFunc: TALBinary64Func;
  public
    constructor Create(const AIdent: AnsiString; AFunc: TALBinary64Func);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprBinary80FuncSym = class(TALExprAbstractFuncSym)
  private
    FFunc: TALBinary80Func;
  public
    constructor Create(const AIdent: AnsiString; AFunc: TALBinary80Func);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprTernaryFuncSym = class(TALExprAbstractFuncSym)
  private
    FFunc: TALTernaryFunc;
  public
    constructor Create(const AIdent: AnsiString; AFunc: TALTernaryFunc);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprTernary32FuncSym = class(TALExprAbstractFuncSym)
  private
    FFunc: TALTernary32Func;
  public
    constructor Create(const AIdent: AnsiString; AFunc: TALTernary32Func);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprTernary64FuncSym = class(TALExprAbstractFuncSym)
  private
    FFunc: TALTernary64Func;
  public
    constructor Create(const AIdent: AnsiString; AFunc: TALTernary64Func);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALExprTernary80FuncSym = class(TALExprAbstractFuncSym)
  private
    FFunc: TALTernary80Func;
  public
    constructor Create(const AIdent: AnsiString; AFunc: TALTernary80Func);
    function Evaluate: TALFloat; override;
    function Compile: TALExprNode; override;
  end;

  TALEasyEvaluator = class(TObject)
  private
    FOwnContext: TALExprHashContext;
    FExtContextSet: TALExprSetContext;
    FInternalContextSet: TALExprSetContext;
  protected
    property InternalContextSet: TALExprSetContext read FInternalContextSet;
  public
    constructor Create;
    destructor Destroy; override;

    //Adds a variable to the internal context. Whenever the variable is found in
    //an expression, its current value will be inserted.
    procedure AddVar(const AName: AnsiString; var AVar: TALFloat32); overload;
    procedure AddVar(const AName: AnsiString; var AVar: TALFloat64); overload;
    procedure AddVar(const AName: AnsiString; var AVar: TALFloat80); overload;

    //Adds a constant to the internal context. Constants are different from variables
    //because sub-expressions made entirely from constants may be evaluated only once
    //(at compile time), and that value used for all subsequent evaluations.
    procedure AddConst(const AName: AnsiString; AConst: TALFloat32); overload;
    procedure AddConst(const AName: AnsiString; AConst: TALFloat64); overload;
    procedure AddConst(const AName: AnsiString; AConst: TALFloat80); overload;

    procedure AddFunc(const AName: AnsiString; AFunc: TALFloat32Func); overload;
    procedure AddFunc(const AName: AnsiString; AFunc: TALFloat64Func); overload;
    procedure AddFunc(const AName: AnsiString; AFunc: TALFloat80Func); overload;
    procedure AddFunc(const AName: AnsiString; AFunc: TALUnary32Func); overload;
    procedure AddFunc(const AName: AnsiString; AFunc: TALUnary64Func); overload;
    procedure AddFunc(const AName: AnsiString; AFunc: TALUnary80Func); overload;
    procedure AddFunc(const AName: AnsiString; AFunc: TALBinary32Func); overload;
    procedure AddFunc(const AName: AnsiString; AFunc: TALBinary64Func); overload;
    procedure AddFunc(const AName: AnsiString; AFunc: TALBinary80Func); overload;
    procedure AddFunc(const AName: AnsiString; AFunc: TALTernary32Func); overload;
    procedure AddFunc(const AName: AnsiString; AFunc: TALTernary64Func); overload;
    procedure AddFunc(const AName: AnsiString; AFunc: TALTernary80Func); overload;
    procedure Remove(const AName: AnsiString);

    procedure Clear; virtual;
    property ExtContextSet: TALExprSetContext read FExtContextSet;
  end;

  TALEvaluator = class(TALEasyEvaluator)
  private
    FLexer:     TALExprSimpleLexer;
    FParser:    TALExprEvalParser;
  public
    constructor Create;
    destructor  Destroy; override;
    function    Evaluate(const AExpr: AnsiString): TALFloat;
  end;

  TALCompiledEvaluator = class(TALEasyEvaluator)
  private
    FExpr: AnsiString;
    FVirtMach: TALExprVirtMach;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Compile(const AExpr: AnsiString);
    function Evaluate: TALFloat;
  end;

resourcestring
  RsALExprEvalRParenExpected = 'Parse error: '')'' expected';
  RsALExprEvalFactorExpected = 'Parse error: Factor expected';
  RsALExprEvalUnknownSymbol  = 'Parse error: Unknown symbol: ''%s''';

  RsALExprEvalFirstArg = 'Parse error: ''('' and function''s first parameter expected';
  RsALExprEvalNextArg  = 'Parse error: '','' and another parameter expected';
  RsALExprEvalEndArgs  = 'Parse error: '')'' to close function''s parameters expected';

implementation

uses
  System.Types,
  System.Math,
  System.Contnrs,
  ALString,
  ALCipher;

{*****************************************************}
function ALEvaluator_Max(A, B: TALFloat80): TALFloat80;
begin
  Result := Max(A, B);
end;

{*****************************************************}
function ALEvaluator_Min(A, B: TALFloat80): TALFloat80;
begin
  Result := Min(A, B);
end;

{***************************************}
procedure ALClearObjectList(List: TList);
var
  I: Integer;
begin
  if List <> nil then
  begin
    for I := List.Count - 1 downto 0 do
    begin
      if List[I] <> nil then
      begin
        if TObject(List[I]) is TList then
        begin
          // recursively delete TList sublists
          ALClearObjectList(TList(List[I]));
        end;
        TObject(List[I]).Free;
        if (not (List is TComponentList))
          and ((not(List is TObjectList)) or not TObjectList(List).OwnsObjects) then
          List[I] := nil;
      end;
    end;
    List.Clear;
  end;
end;

{******************************************}
procedure ALFreeObjectList(var List: TList);
begin
  if List <> nil then
  begin
    ALClearObjectList(List);
    FreeAndNil(List);
  end;
end;

{******************************************************}
function ALCharIsWhiteSpace(const C: AnsiChar): Boolean;
const _AnsiTab            = AnsiChar(#9);
      _AnsiLineFeed       = AnsiChar(#10);
      _AnsiVerticalTab    = AnsiChar(#11);
      _AnsiFormFeed       = AnsiChar(#12);
      _AnsiCarriageReturn = AnsiChar(#13);
      _AnsiSpace          = AnsiChar(' ');
begin
  Result := (C = _AnsiTab) or (C = _AnsiLineFeed) or (C = _AnsiVerticalTab) or
            (C = _AnsiFormFeed) or (C = _AnsiCarriageReturn) or (C = _AnsiSpace);
end;

{*****************************************************************}
function ALCharIsValidIdentifierLetter(const C: AnsiChar): Boolean;
begin
  case C of
    '0'..'9', 'A'..'Z', 'a'..'z', '_':
      Result := True;
  else
    Result := False;
  end;
end;

{*************************************************}
function ALCharIsDigit(const C: AnsiChar): Boolean;
begin
  Result := C in ['0'..'9'];
end;

{*************************************************************}
constructor TALExprHashContext.Create(ACaseSensitive: Boolean);
begin
  inherited Create;
  FHashMap := TALAVLStringList.Create(true);
  FHashMap.CaseSensitive := ACaseSensitive;
end;

{************************************}
destructor TALExprHashContext.Destroy;
begin
  FHashMap.Free;
  inherited Destroy;
end;

{****************************************************}
procedure TALExprHashContext.Add(ASymbol: TALExprSym);
begin
  FHashMap.AddObject(ASymbol.Ident, ASymbol);
end;

{***********************************************************}
procedure TALExprHashContext.Remove(const AName: AnsiString);
Var aIdx: integer;
begin
  aIdx := FHashMap.IndexOf(AName);
  if aIdx >= 0 then FHashMap.Delete(aIdx);
end;

{********************************************************************}
function TALExprHashContext.Find(const AName: AnsiString): TALExprSym;
Var aIdx: integer;
begin
  Result := nil;
  aIdx := FHashMap.IndexOf(AName);
  if aIdx >= 0 then result := TALExprSym(FHashMap.Objects[aIdx]);
end;

{***********************************************************}
constructor TALExprSetContext.Create(AOwnsContexts: Boolean);
begin
  inherited Create;
  FOwnsContexts := AOwnsContexts;
  FList := TList.Create;
end;

{***********************************}
destructor TALExprSetContext.Destroy;
begin
  if FOwnsContexts then
    ALClearObjectList(FList);
  FList.Free;
  inherited Destroy;
end;

{********************************************************}
procedure TALExprSetContext.Add(AContext: TALExprContext);
begin
  FList.Add(AContext);
end;

{**************************************************}
procedure TALExprSetContext.Delete(AIndex: Integer);
begin
  if FOwnsContexts then
    TObject(FList[AIndex]).Free;
  FList.Delete(AIndex);
end;

{***************************************************************************}
function TALExprSetContext.Extract(AContext: TALExprContext): TALExprContext;
begin
  Result := AContext;
  FList.Remove(AContext);
end;

{*******************************************************************}
function TALExprSetContext.Find(const AName: AnsiString): TALExprSym;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
  begin
    Result := Contexts[I].Find(AName);
    if Result <> nil then
      Break;
  end;
end;

{**********************************************************************}
function TALExprSetContext.GetContexts(AIndex: Integer): TALExprContext;
begin
  Result := TALExprContext(FList[AIndex]);
end;

{*******************************************}
function TALExprSetContext.GetCount: Integer;
begin
  Result := FList.Count;
end;

{***********************************************************}
procedure TALExprSetContext.Remove(AContext: TALExprContext);
begin
  FList.Remove(AContext);
  if FOwnsContexts then
    AContext.Free;
end;

{******************************************************}
constructor TALExprSym.Create(const AIdent: AnsiString);
begin
  inherited Create;
  FIdent := AIdent;
end;

{******************************}
constructor TALExprLexer.Create;
begin
  inherited Create;
  Reset;
end;

{***************************}
procedure TALExprLexer.Reset;
begin
  NextTok;
end;

{**********************************************************************************************}
constructor TALExprCompileParser.Create(ALexer: TALExprLexer; ANodeFactory: TALExprNodeFactory);
begin
  inherited Create;
  FLexer := ALexer;
  FNodeFactory := ANodeFactory;
end;

{*************************************************}
function TALExprCompileParser.Compile: TALExprNode;
begin
  Result := CompileExprLevel0(False);
end;

{***************************************************************************}
function TALExprCompileParser.CompileExprLevel0(ASkip: Boolean): TALExprNode;
begin
  Result := CompileExprLevel1(ASkip);

  { Utilize some of these compound instructions to test DAG optimization
    techniques later on.

    Playing a few games after much hard work, too.
    Functional programming is fun! :-> BJK }
  while True do
    case Lexer.CurrTok of
      etEqualTo: // =
        Result := NodeFactory.CompareEqual(Result, CompileExprLevel1(True));
      etNotEqual: // <>
        Result := NodeFactory.CompareNotEqual(Result, CompileExprLevel1(True));
      etLessThan: // <
        Result := NodeFactory.CompareLess(Result, CompileExprLevel1(True));
      etLessEqual: // <=
        Result := NodeFactory.CompareLessEqual(Result, CompileExprLevel1(True));
      etGreaterThan: // >
        Result := NodeFactory.CompareGreater(Result, CompileExprLevel1(True));
      etGreaterEqual: // >=
        Result := NodeFactory.CompareGreaterEqual(Result, CompileExprLevel1(True));
      etIdentifier: // cmp
        if ALSameText(Lexer.TokenAsString, 'cmp') then
          Result := NodeFactory.Compare(Result, CompileExprLevel1(True))
        else
          Break;
    else
      Break;
    end;
end;

{***************************************************************************}
function TALExprCompileParser.CompileExprLevel1(ASkip: Boolean): TALExprNode;
begin
  Result := CompileExprLevel2(ASkip);

  while True do
    case Lexer.CurrTok of
      etPlus:
        Result := NodeFactory.Add(Result, CompileExprLevel2(True));
      etMinus:
        Result := NodeFactory.Subtract(Result, CompileExprLevel2(True));
      etIdentifier: // or, xor, bor, bxor
        if ALSameText(Lexer.TokenAsString, 'or') then
          Result := NodeFactory.LogicalOr(Result, CompileExprLevel2(True))
        else
        if ALSameText(Lexer.TokenAsString, 'xor') then
          Result := NodeFactory.LogicalXor(Result, CompileExprLevel2(True))
        else
        if ALSameText(Lexer.TokenAsString, 'bor') then
          Result := NodeFactory.BitwiseOr(Result, CompileExprLevel2(True))
        else
        if ALSameText(Lexer.TokenAsString, 'bxor') then
          Result := NodeFactory.BitwiseXor(Result, CompileExprLevel2(True))
        else
          Break;
    else
      Break;
    end;
end;

{***************************************************************************}
function TALExprCompileParser.CompileExprLevel2(ASkip: Boolean): TALExprNode;
begin
  Result := CompileExprLevel3(ASkip);

  while True do
    case Lexer.CurrTok of
      etAsterisk:
        Result := NodeFactory.Multiply(Result, CompileExprLevel3(True));
      etForwardSlash:
        Result := NodeFactory.Divide(Result, CompileExprLevel3(True));
      etIdentifier: // div, mod, and, shl, shr, band
        if ALSameText(Lexer.TokenAsString, 'div') then
          Result := NodeFactory.IntegerDivide(Result, CompileExprLevel3(True))
        else
        if ALSameText(Lexer.TokenAsString, 'mod') then
          Result := NodeFactory.Modulo(Result, CompileExprLevel3(True))
        else
        if ALSameText(Lexer.TokenAsString, 'and') then
          Result := NodeFactory.LogicalAnd(Result, CompileExprLevel3(True))
        else
        if ALSameText(Lexer.TokenAsString, 'shl') then
          Result := NodeFactory.ShiftLeft(Result, CompileExprLevel3(True))
        else
        if ALSameText(Lexer.TokenAsString, 'shr') then
          Result := NodeFactory.ShiftRight(Result, CompileExprLevel3(True))
        else
        if ALSameText(Lexer.TokenAsString, 'band') then
          Result := NodeFactory.BitwiseAnd(Result, CompileExprLevel3(True))
        else
          Break;
    else
      Break;
    end;
end;

{***************************************************************************}
function TALExprCompileParser.CompileExprLevel3(ASkip: Boolean): TALExprNode;
begin
  if ASkip then
    Lexer.NextTok;

  case Lexer.CurrTok of
    etPlus:
      Result := CompileExprLevel3(True);
    etMinus:
      Result := NodeFactory.Negate(CompileExprLevel3(True));
    etIdentifier: // not, bnot
      if ALSameText(Lexer.TokenAsString, 'not') then
        Result := NodeFactory.LogicalNot(CompileExprLevel3(True))
      else
      if ALSameText(Lexer.TokenAsString, 'bnot') then
        Result := NodeFactory.BitwiseNot(CompileExprLevel3(True))
      else
        Result := CompileFactor;
  else
    Result := CompileFactor;
  end;
end;

{*******************************************************}
function TALExprCompileParser.CompileFactor: TALExprNode;
begin
  case Lexer.CurrTok of
    etIdentifier:
      Result := CompileIdentFactor;
    etLParen:
      begin
        Result := CompileExprLevel0(True);
        if Lexer.CurrTok <> etRParen then
          raise EALExprEvalError.CreateRes(@RsALExprEvalRParenExpected);
        Lexer.NextTok;
      end;
    etNumber:
      begin
        Result := NodeFactory.LoadConst(Lexer.TokenAsNumber);
        Lexer.NextTok;
      end;
  else
    raise EALExprEvalError.CreateRes(@RsALExprEvalFactorExpected);
  end;
end;

{************************************************************}
function TALExprCompileParser.CompileIdentFactor: TALExprNode;
var
  Sym: TALExprSym;
  oldCompileParser: TALExprCompileParser;
  oldLexer: TALExprLexer;
  oldNodeFactory: TALExprNodeFactory;
begin
  { find symbol }
  if FContext = nil then
    raise EALExprEvalError.CreateResFmt(@RsALExprEvalUnknownSymbol,
      [Lexer.TokenAsString]);
  Sym := FContext.Find(Lexer.TokenAsString);
  if Sym = nil then
    raise EALExprEvalError.CreateResFmt(@RsALExprEvalUnknownSymbol,
      [Lexer.TokenAsString]);

  Lexer.NextTok;

  { set symbol properties }
  oldCompileParser := Sym.CompileParser;
  oldLexer := Sym.Lexer;
  oldNodeFactory := Sym.NodeFactory;
  Sym.FLexer := Lexer;
  Sym.FCompileParser := Self;
  Sym.FNodeFactory := NodeFactory;
  try
    { compile symbol }
    Result := Sym.Compile;
  finally
    Sym.FLexer := oldLexer;
    Sym.FCompileParser := oldCompileParser;
    Sym.FNodeFactory := oldNodeFactory;
  end;
end;

{*********************************************************}
constructor TALExprEvalParser.Create(ALexer: TALExprLexer);
begin
  inherited Create;
  FLexer := ALexer;
end;

{********************************************}
function TALExprEvalParser.Evaluate: TALFloat;
begin
  Result := EvalExprLevel0(False);

  if (Lexer.CurrTok <> etEof) then
  begin
    raise EALExprEvalError.CreateResFmt(@RsALExprEvalUnknownSymbol,
      [Lexer.TokenAsString]);
  end;
end;

{******************************************************************}
function TALExprEvalParser.EvalExprLevel0(ASkip: Boolean): TALFloat;
var
  RightValue: TALFloat;
begin
  Result := EvalExprLevel1(ASkip);

  while True do
    case Lexer.CurrTok of
      etEqualTo: // =
        if Result = EvalExprLevel1(True) then
          Result := 1.0
        else
          Result := 0.0;
      etNotEqual: // <>
        if Result <> EvalExprLevel1(True) then
          Result := 1.0
        else
          Result := 0.0;
      etLessThan: // <
        if Result < EvalExprLevel1(True) then
          Result := 1.0
        else
          Result := 0.0;
      etLessEqual: // <=
        if Result <= EvalExprLevel1(True) then
          Result := 1.0
        else
          Result := 0.0;
      etGreaterThan: // >
        if Result > EvalExprLevel1(True) then
          Result := 1.0
        else
          Result := 0.0;
      etGreaterEqual: // >=
        if Result >= EvalExprLevel1(True) then
          Result := 1.0
        else
          Result := 0.0;
      etIdentifier: // cmp
        if ALSameText(Lexer.TokenAsString, 'cmp') then
        begin
          RightValue := EvalExprLevel1(True);
          if Result > RightValue then
            Result := 1.0
          else
          if Result = RightValue then
            Result := 0.0
          else
            Result := -1.0;
        end
        else
          Break;
    else
      Break;
    end;
end;

{******************************************************************}
function TALExprEvalParser.EvalExprLevel1(ASkip: Boolean): TALFloat;
begin
  Result := EvalExprLevel2(ASkip);

  while True do
    case Lexer.CurrTok of
      etPlus:
        Result := Result + EvalExprLevel2(True);
      etMinus:
        Result := Result - EvalExprLevel2(True);
      etIdentifier: // or, xor, bor, bxor
        if ALSameText(Lexer.TokenAsString, 'or') then
        begin
          if (EvalExprLevel2(True) <> 0) or (Result <> 0) then // prevent boolean optimisations, EvalTerm must be called
            Result := 1.0
          else
            Result := 0.0;
        end
        else
        if ALSameText(Lexer.TokenAsString, 'xor') then
        begin
          if (Result <> 0) xor (EvalExprLevel2(True) <> 0) then
            Result := 1.0
          else
            result := 0.0;
        end
        else
        if ALSameText(Lexer.TokenAsString, 'bor') then
          Result := Round(Result) or Round(EvalExprLevel2(True))
        else
        if ALSameText(Lexer.TokenAsString, 'bxor') then
          Result := Round(Result) xor Round(EvalExprLevel2(True))
        else
          Break;
    else
      Break;
    end;
end;

{******************************************************************}
function TALExprEvalParser.EvalExprLevel2(ASkip: Boolean): TALFloat;
var OpenTok: TALExprToken;
    CloseTok: TALExprToken;
    OpenTokCount: integer;
begin
  Result := EvalExprLevel3(ASkip);

  while True do
    case Lexer.CurrTok of
      etAsterisk:
        Result := Result * EvalExprLevel3(True);
      etForwardSlash:
        Result := Result / EvalExprLevel3(True);
      etIdentifier: // div, mod, and, shl, shr, band, in
        if ALSameText(Lexer.TokenAsString, 'in') then
        begin

          Lexer.NextTok;

          OpenTok := Lexer.CurrTok;
          if OpenTok = etLParen then CloseTok := etRParen
          else if OpenTok = etLBracket then CloseTok := etRBracket
          else raise EALExprEvalError.CreateRes(@RsALExprEvalFirstArg);

          while true do begin

            if result = EvalExprLevel3(True) then begin

              result := 1.0;

              OpenTokCount := 1;
              while True do begin

                if Lexer.CurrTok = OpenTok then inc(OpenTokCount)
                else if Lexer.CurrTok = CloseTok then begin
                  dec(OpenTokCount);
                  if OpenTokCount = 0 then begin
                    Lexer.NextTok;
                    break;
                  end;
                end
                else if Lexer.CurrTok = etEOF then
                  raise EALExprEvalError.CreateRes(@RsALExprEvalEndArgs);

                Lexer.NextTok(false);

              end;

              break;

            end

            else begin
              if Lexer.CurrTok = CloseTok then begin
                result := 0.0;
                Lexer.NextTok;
                break;
              end
              else if Lexer.CurrTok <> etComma then
                raise EALExprEvalError.CreateRes(@RsALExprEvalNextArg);
            end;

          end;

        end
        else
        if ALSameText(Lexer.TokenAsString, 'div') then
          Result := Round(Result) div Round(EvalExprLevel3(True))
        else
        if ALSameText(Lexer.TokenAsString, 'mod') then
          Result := Round(Result) mod Round(EvalExprLevel3(True))
        else
        if ALSameText(Lexer.TokenAsString, 'and') then
        begin
          if (EvalExprLevel3(True) <> 0) and (Result <> 0) then // prevent boolean optimisations, EvalTerm must be called
            Result := 1.0
          else
            Result := 0.0;
        end
        else
        if ALSameText(Lexer.TokenAsString, 'shl') then
          Result := Round(Result) shl Round(EvalExprLevel3(True))
        else
        if ALSameText(Lexer.TokenAsString, 'shr') then
          Result := Round(Result) shr Round(EvalExprLevel3(True))
        else
        if ALSameText(Lexer.TokenAsString, 'band') then
          Result := Round(Result) and Round(EvalExprLevel3(True))
        else
          Break;
    else
      Break;
    end;
end;

{******************************************************************}
function TALExprEvalParser.EvalExprLevel3(ASkip: Boolean): TALFloat;
begin
  if ASkip then
    Lexer.NextTok;

  case Lexer.CurrTok of
    etPlus:
      Result := EvalExprLevel3(True);
    etMinus:
      Result := -EvalExprLevel3(True);
    etIdentifier: // not, bnot
      if ALSameText(Lexer.TokenAsString, 'not') then
      begin
        if EvalExprLevel3(True) <> 0.0 then
          Result := 0.0
        else
          Result := 1.0;
      end
      else
      if ALSameText(Lexer.TokenAsString, 'bnot') then
        Result := not Round(EvalExprLevel3(True))
      else
        Result := EvalFactor;
  else
    Result := EvalFactor;
  end;
end;

{**********************************************}
function TALExprEvalParser.EvalFactor: TALFloat;
begin
  case Lexer.CurrTok of
    etIdentifier:
      Result := EvalIdentFactor;
    etLParen:
      begin
        Result := EvalExprLevel0(True);
        if Lexer.CurrTok <> etRParen then
          raise EALExprEvalError.CreateRes(@RsALExprEvalRParenExpected);
        Lexer.NextTok;
      end;
    etNumber:
      begin
        Result := Lexer.TokenAsNumber;
        Lexer.NextTok;
      end;
  else
    raise EALExprEvalError.CreateRes(@RsALExprEvalFactorExpected);
  end;
end;

{***************************************************}
function TALExprEvalParser.EvalIdentFactor: TALFloat;
var
  Sym: TALExprSym;
  oldEvalParser: TALExprEvalParser;
  oldLexer: TALExprLexer;
begin
  { find symbol }
  if Context = nil then
    raise EALExprEvalError.CreateResFmt(@RsALExprEvalUnknownSymbol,
      [Lexer.TokenAsString]);
  Sym := FContext.Find(Lexer.TokenAsString);
  if Sym = nil then
    raise EALExprEvalError.CreateResFmt(@RsALExprEvalUnknownSymbol,
      [Lexer.TokenAsString]);

  Lexer.NextTok;

  { set symbol properties }
  oldEvalParser := Sym.FEvalParser;
  oldLexer := Sym.Lexer;
  Sym.FLexer := Lexer;
  Sym.FEvalParser := Self;
  try
    { evaluate symbol }
    Result := Sym.Evaluate;
  finally
    Sym.FLexer := oldLexer;
    Sym.FEvalParser := oldEvalParser;
  end;
end;

{************************************************************}
constructor TALExprSimpleLexer.Create(const ABuf: AnsiString);
begin
  FBuf := ABuf;
  inherited Create;
end;

{******************************************************************}
procedure TALExprSimpleLexer.NextTok(const EvalTok: Boolean = True);
const
  CharToTokenMap: array [AnsiChar] of TALExprToken =
  (
    {#0..#31}
    etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid,
    {#32} etInvalid,
    {#33} etBang, {#34} etDoubleQuote, {#35} etHash, {#36} etDollar,
    {#37} etPercent, {#38} etAmpersand, {#39} etSingleQuote, {#40} etLParen,
    {#41} etRParen, {#42} etAsterisk, {#43} etPlus, {#44} etComma,
    {#45} etMinus, {#46} etDot, {#47} etForwardSlash,
    // 48..57 - numbers...
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid,
    {#58} etColon, {#59} etSemiColon, {#60} etLessThan, {#61} etEqualTo,
    {#62} etGreaterThan, {#63} etQuestion, {#64} etAt,
    // 65..90 - capital letters...
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid,
    {#91} etLBracket, {#92} etBackSlash, {#93} etRBracket, {#94} etArrow,
    etInvalid, // 95 - underscore
    {#96} etBackTick,
    // 97..122 - small letters...
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid,
    {#123} etLBrace,
    {#124} etPipe, {#125} etRBrace, {#126} etTilde, {#127} et127,
    {#128} etEuro, {#129} et129, {#130} et130, {#131} et131,
    {#132} et132, {#133} et133, {#134} et134, {#135} et135,
    {#136} et136, {#137} et137, {#138} et138, {#139} et139,
    {#140} et140, {#141} et141, {#142} et142, {#143} et143,
    {#144} et144, {#145} et145, {#146} et146, {#147} et147,
    {#148} et148, {#149} et149, {#150} et150, {#151} et151,
    {#152} et152, {#153} et153, {#154} et154, {#155} et155,
    {#156} et156, {#157} et157, {#158} et158, {#159} et159,
    {#160} et160, {#161} et161, {#162} et162, {#163} et163,
    {#164} et164, {#165} et165, {#166} et166, {#167} et167,
    {#168} et168, {#169} et169, {#170} et170, {#171} et171,
    {#172} et172, {#173} et173, {#174} et174, {#175} et175,
    {#176} et176, {#177} et177, {#178} et178, {#179} et179,
    {#180} et180, {#181} et181, {#182} et182, {#183} et183,
    {#184} et184, {#185} et185, {#186} et186, {#187} et187,
    {#188} et188, {#189} et189, {#190} et190, {#191} et191,
    {#192} et192, {#193} et193, {#194} et194, {#195} et195,
    {#196} et196, {#197} et197, {#198} et198, {#199} et199,
    {#200} et200, {#201} et201, {#202} et202, {#203} et203,
    {#204} et204, {#205} et205, {#206} et206, {#207} et207,
    {#208} et208, {#209} et209, {#210} et210, {#211} et211,
    {#212} et212, {#213} et213, {#214} et214, {#215} et215,
    {#216} et216, {#217} et217, {#218} et218, {#219} et219,
    {#220} et220, {#221} et221, {#222} et222, {#223} et223,
    {#224} et224, {#225} et225, {#226} et226, {#227} et227,
    {#228} et228, {#229} et229, {#230} et230, {#231} et231,
    {#232} et232, {#233} et233, {#234} et234, {#235} et235,
    {#236} et236, {#237} et237, {#238} et238, {#239} et239,
    {#240} et240, {#241} et241, {#242} et242, {#243} et243,
    {#244} et244, {#245} et245, {#246} et246, {#247} et247,
    {#248} et248, {#249} et249, {#250} et250, {#251} et251,
    {#252} et252, {#253} et253, {#254} et254, {#255} et255
  );
var
  { register variable optimization }
  cp: PAnsiChar;
  start: PAnsiChar;
  InSingleQuote, InDoubleQuote: Boolean;
begin
  cp := FCurrPos;

  { skip whitespace }
  while ALCharIsWhiteSpace(cp^) do
    Inc(cp);

  { determine token type }
  case cp^ of
    #0:
      FCurrTok := etEof;
    '"', '''':
      begin

        //Replace all the strings by their CRC - to compare them as numbers
        //but replace empty string '' by 0 instead of the CRC of an empty string
        start := cp;
        InSingleQuote := cp^ = '''';
        InDoubleQuote := cp^ = '"';
        inc(cp);
        While (InSingleQuote or InDoubleQuote) and (cp^ <> #0) do begin

          if (cp^ = '"') then begin
            InDoubleQuote := (not InDoubleQuote) and (not InSingleQuote);
            inc(cp);
            if (cp^ = '"') then begin
              inc(cp);
              InDoubleQuote := (not InDoubleQuote) and (not InSingleQuote);
            end;
          end
          else if (cp^ = '''') then begin
            InSingleQuote := (not InSingleQuote) and (not InDoubleQuote);
            inc(cp);
            if (cp^ = '''') then begin
              inc(cp);
              InSingleQuote := (not InSingleQuote) and (not InDoubleQuote);
            end;
          end
          else inc(cp);

        end;

        if InSingleQuote or InDoubleQuote then FCurrTok := etInvalid
        else begin

          { evaluate number }
          if EvalTok then begin
            SetString(FTokenAsString, start, cp - start);
            FTokenAsString := ALDequotedStr(FTokenAsString, FTokenAsString[1]);
            if FTokenAsString = '' then FTokenAsNumber := 0
            else FTokenAsNumber := ALFnv1aInt64(FTokenAsString);
          end;

          FCurrTok := etNumber;

        end;

      end;
    'a'..'z', 'A'..'Z', '_':
      begin
        start := cp;
        Inc(cp);
        while ALCharIsValidIdentifierLetter(cp^) do
          Inc(cp);
        if EvalTok then SetString(FTokenAsString, start, cp - start);
        FCurrTok := etIdentifier;
      end;
    '0'..'9':
      begin
        start := cp;

        { read in integer part of mantissa }
        while ALCharIsDigit(cp^) do
          Inc(cp);

        { check for and read in fraction part of mantissa }
        if (cp^ = '.') {or (cp^ = JclFormatSettings.DecimalSeparator)} then
        begin
          Inc(cp);
          while ALCharIsDigit(cp^) do
            Inc(cp);
        end;

        { check for and read in exponent }
        if (cp^ = 'e') or (cp^ = 'E') then
        begin
          Inc(cp);
          if (cp^ = '+') or (cp^ = '-') then
            Inc(cp);
          while ALCharIsDigit(cp^) do
            Inc(cp);
        end;

        { evaluate number }
        if EvalTok then begin
          SetString(FTokenAsString, start, cp - start);
          FTokenAsNumber := ALStrToFloat(FTokenAsString, ALDefaultFormatSettings);
        end;

        FCurrTok := etNumber;
      end;
    '<':
      begin
        Inc(cp);
        case cp^ of
          '=':
            begin
              FCurrTok := etLessEqual;
              Inc(cp);
            end;
          '>':
            begin
              FCurrTok := etNotEqual;
              Inc(cp);
            end;
        else
          FCurrTok := etLessThan;
        end;
      end;
    '>':
      begin
        Inc(cp);
        if cp^ = '=' then
        begin
          FCurrTok := etGreaterEqual;
          Inc(cp);
        end
        else
          FCurrTok := etGreaterThan;
      end;
  else
    { map character to token }
    if Word(cp^) < 256 then
      FCurrTok := CharToTokenMap[AnsiChar(cp^)]
    else
      FCurrTok := etInvalid;
    Inc(cp);
  end;

  FCurrPos := cp;
end;

{*********************************}
procedure TALExprSimpleLexer.Reset;
begin
  FCurrPos := PAnsiChar(FBuf);
  inherited Reset;
end;

{**********************************************************}
procedure TALExprSimpleLexer.SetBuf(const ABuf: AnsiString);
begin
  FBuf := ABuf;
  Reset;
end;

{*******************************************************************}
constructor TALExprNode.Create(const ADepList: array of TALExprNode);
var
  I: Integer;
begin
  inherited Create;
  FDepList := TList.Create;
  for I := Low(ADepList) to High(ADepList) do
    AddDep(ADepList[I]);
end;

{*****************************}
destructor TALExprNode.Destroy;
begin
  FDepList.Free;
  inherited Destroy;
end;

{**********************************************}
procedure TALExprNode.AddDep(ADep: TALExprNode);
begin
  FDepList.Add(ADep);
end;

{****************************************}
function TALExprNode.GetDepCount: Integer;
begin
  Result := FDepList.Count;
end;

{*********************************************************}
function TALExprNode.GetDeps(AIndex: Integer): TALExprNode;
begin
  Result := TALExprNode(FDepList[AIndex]);
end;

{*****************************************************************}
function TALExprNodeFactory.LoadVar(ALoc: PALFloat32): TALExprNode;
begin
  Result := LoadVar32(ALoc);
end;

{*****************************************************************}
function TALExprNodeFactory.LoadVar(ALoc: PALFloat64): TALExprNode;
begin
  Result := LoadVar64(ALoc);
end;

{*****************************************************************}
function TALExprNodeFactory.LoadVar(ALoc: PALFloat80): TALExprNode;
begin
  Result := LoadVar80(ALoc);
end;

{*********************************************************************}
function TALExprNodeFactory.LoadConst(AValue: TALFloat32): TALExprNode;
begin
  Result := LoadConst32(AValue);
end;

{*********************************************************************}
function TALExprNodeFactory.LoadConst(AValue: TALFloat64): TALExprNode;
begin
  Result := LoadConst64(AValue);
end;

{*********************************************************************}
function TALExprNodeFactory.LoadConst(AValue: TALFloat80): TALExprNode;
begin
  Result := LoadConst80(AValue);
end;

{******************************}
constructor TALEvaluator.Create;
begin
  inherited Create;

  FLexer := TALExprSimpleLexer.Create('');
  FParser := TALExprEvalParser.Create(FLexer);

  FParser.Context := InternalContextSet;

  addfunc('max',ALEvaluator_max);
  addfunc('min',ALEvaluator_min);
end;

{******************************}
destructor TALEvaluator.Destroy;
begin
  FParser.Free;
  FLexer.Free;
  inherited Destroy;
end;

{****************************************************************}
function TALEvaluator.Evaluate(const AExpr: AnsiString): TALFloat;
begin
  FLexer.Buf := AExpr;
  Result := FParser.Evaluate;
end;

{************************************************}
function TALExprVirtMachOp.GetOutputLoc: PALFloat;
begin
  Result := @FOutput;
end;

{ Virtual machine operators follow }

type

  {-------------------------------}
  { abstract base for var readers }
  TALExprVarVmOp = class(TALExprVirtMachOp)
  private
    FVarLoc: Pointer;
  public
    constructor Create(AVarLoc: Pointer);
  end;

  {-----------------}
  { the var readers }
  TALExprVar32VmOp = class(TALExprVarVmOp)
  public
    procedure Execute; override;
  end;

  {--------------------------------------}
  TALExprVar64VmOp = class(TALExprVarVmOp)
  public
    procedure Execute; override;
  end;

  {--------------------------------------}
  TALExprVar80VmOp = class(TALExprVarVmOp)
  public
    procedure Execute; override;
  end;

  {------------------}
  { the const holder }
  TALExprConstVmOp = class(TALExprVirtMachOp)
  public
    constructor Create(AValue: TALFloat);
    { null function }
    procedure Execute; override;
  end;

  {-------------------------}
  { abstract unary operator }
  TALExprUnaryVmOp = class(TALExprVirtMachOp)
  protected
    FInput: PALFloat;
  public
    constructor Create(AInput: PALFloat);
    property Input: PALFloat read FInput write FInput;
  end;

  {------------------------------------------------}
  TALExprUnaryVmOpClass = class of TALExprUnaryVmOp;

  {--------------------------}
  { abstract binary operator }
  TALExprBinaryVmOp = class(TALExprVirtMachOp)
  protected
    FLeft: PALFloat;
    FRight: PALFloat;
  public
    constructor Create(ALeft, ARight: PALFloat);
    property Left: PALFloat read FLeft write FLeft;
    property Right: PALFloat read FRight write FRight;
  end;

  {--------------------------------------------------}
  TALExprBinaryVmOpClass = class of TALExprBinaryVmOp;

  { the 4 basic binary operators }

  {---------------------------------------}
  TALExprAddVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {--------------------------------------------}
  TALExprSubtractVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {--------------------------------------------}
  TALExprMultiplyVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {------------------------------------------}
  TALExprDivideVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {-------------------------------------------}
  TALExprCompareVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {-------------------------------------------}
  TALExprGreaterVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {------------------------------------------------}
  TALExprGreaterEqualVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {----------------------------------------}
  TALExprLessVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {---------------------------------------------}
  TALExprLessEqualVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {-----------------------------------------}
  TALExprEqualVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {--------------------------------------------}
  TALExprNotEqualVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {-------------------------------------------------}
  TALExprIntegerDivideVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {------------------------------------------}
  TALExprModuloVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {---------------------------------------------}
  TALExprShiftLeftVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {----------------------------------------------}
  TALExprShiftRightVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {----------------------------------------------}
  TALExprBitwiseAndVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {---------------------------------------------}
  TALExprBitwiseOrVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {----------------------------------------------}
  TALExprBitwiseXorVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {----------------------------------------------}
  TALExprLogicalAndVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {---------------------------------------------}
  TALExprLogicalOrVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  {----------------------------------------------}
  TALExprLogicalXorVmOp = class(TALExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  { the unary operators }

  {-----------------------------------------}
  TALExprNegateVmOp = class(TALExprUnaryVmOp)
  public
    procedure Execute; override;
  end;

  {---------------------------------------------}
  TALExprLogicalNotVmOp = class(TALExprUnaryVmOp)
  public
    procedure Execute; override;
  end;

  {---------------------------------------------}
  TALExprBitwiseNotVmOp = class(TALExprUnaryVmOp)
  public
    procedure Execute; override;
  end;

  { function calls }

  {---------------------------------------------}
  TALExprCallFloatVmOp = class(TALExprVirtMachOp)
  private
    FFunc: TALFloatFunc;
  public
    constructor Create(AFunc: TALFloatFunc);
    procedure Execute; override;
  end;

  {-----------------------------------------------}
  TALExprCallFloat32VmOp = class(TALExprVirtMachOp)
  private
    FFunc: TALFloat32Func;
  public
    constructor Create(AFunc: TALFloat32Func);
    procedure Execute; override;
  end;

  {-----------------------------------------------}
  TALExprCallFloat64VmOp = class(TALExprVirtMachOp)
  private
    FFunc: TALFloat64Func;
  public
    constructor Create(AFunc: TALFloat64Func);
    procedure Execute; override;
  end;

  {-----------------------------------------------}
  TALExprCallFloat80VmOp = class(TALExprVirtMachOp)
  private
    FFunc: TALFloat80Func;
  public
    constructor Create(AFunc: TALFloat80Func);
    procedure Execute; override;
  end;

  {---------------------------------------------}
  TALExprCallUnaryVmOp = class(TALExprVirtMachOp)
  private
    FFunc: TALUnaryFunc;
    FX: PALFloat;
  public
    constructor Create(AFunc: TALUnaryFunc; X: PALFloat);
    procedure Execute; override;
  end;

  {-----------------------------------------------}
  TALExprCallUnary32VmOp = class(TALExprVirtMachOp)
  private
    FFunc: TALUnary32Func;
    FX: PALFloat;
  public
    constructor Create(AFunc: TALUnary32Func; X: PALFloat);
    procedure Execute; override;
  end;

  {-----------------------------------------------}
  TALExprCallUnary64VmOp = class(TALExprVirtMachOp)
  private
    FFunc: TALUnary64Func;
    FX: PALFloat;
  public
    constructor Create(AFunc: TALUnary64Func; X: PALFloat);
    procedure Execute; override;
  end;

  {-----------------------------------------------}
  TALExprCallUnary80VmOp = class(TALExprVirtMachOp)
  private
    FFunc: TALUnary80Func;
    FX: PALFloat;
  public
    constructor Create(AFunc: TALUnary80Func; X: PALFloat);
    procedure Execute; override;
  end;

  {----------------------------------------------}
  TALExprCallBinaryVmOp = class(TALExprVirtMachOp)
  private
    FFunc: TALBinaryFunc;
    FX, FY: PALFloat;
  public
    constructor Create(AFunc: TALBinaryFunc; X, Y: PALFloat);
    procedure Execute; override;
  end;

  {------------------------------------------------}
  TALExprCallBinary32VmOp = class(TALExprVirtMachOp)
  private
    FFunc: TALBinary32Func;
    FX, FY: PALFloat;
  public
    constructor Create(AFunc: TALBinary32Func; X, Y: PALFloat);
    procedure Execute; override;
  end;

  {------------------------------------------------}
  TALExprCallBinary64VmOp = class(TALExprVirtMachOp)
  private
    FFunc: TALBinary64Func;
    FX, FY: PALFloat;
  public
    constructor Create(AFunc: TALBinary64Func; X, Y: PALFloat);
    procedure Execute; override;
  end;

  {------------------------------------------------}
  TALExprCallBinary80VmOp = class(TALExprVirtMachOp)
  private
    FFunc: TALBinary80Func;
    FX, FY: PALFloat;
  public
    constructor Create(AFunc: TALBinary80Func; X, Y: PALFloat);
    procedure Execute; override;
  end;

  {-----------------------------------------------}
  TALExprCallTernaryVmOp = class(TALExprVirtMachOp)
  private
    FFunc: TALTernaryFunc;
    FX, FY, FZ: PALFloat;
  public
    constructor Create(AFunc: TALTernaryFunc; X, Y, Z: PALFloat);
    procedure Execute; override;
  end;

  {-------------------------------------------------}
  TALExprCallTernary32VmOp = class(TALExprVirtMachOp)
  private
    FFunc: TALTernary32Func;
    FX, FY, FZ: PALFloat;
  public
    constructor Create(AFunc: TALTernary32Func; X, Y, Z: PALFloat);
    procedure Execute; override;
  end;

  {-------------------------------------------------}
  TALExprCallTernary64VmOp = class(TALExprVirtMachOp)
  private
    FFunc: TALTernary64Func;
    FX, FY, FZ: PALFloat;
  public
    constructor Create(AFunc: TALTernary64Func; X, Y, Z: PALFloat);
    procedure Execute; override;
  end;

  {-------------------------------------------------}
  TALExprCallTernary80VmOp = class(TALExprVirtMachOp)
  private
    FFunc: TALTernary80Func;
    FX, FY, FZ: PALFloat;
  public
    constructor Create(AFunc: TALTernary80Func; X, Y, Z: PALFloat);
    procedure Execute; override;
  end;

{*********************************}
procedure TALExprVar32VmOp.Execute;
begin
  FOutput := PALFloat32(FVarLoc)^;
end;

{*********************************}
procedure TALExprVar64VmOp.Execute;
begin
  FOutput := PALFloat64(FVarLoc)^;
end;

{*********************************}
procedure TALExprVar80VmOp.Execute;
begin
  FOutput := PALFloat80(FVarLoc)^;
end;

{****************************************************}
constructor TALExprConstVmOp.Create(AValue: TALFloat);
begin
  inherited Create;
  FOutput := AValue;
end;

{*********************************}
procedure TALExprConstVmOp.Execute;
begin
end;

{****************************************************}
constructor TALExprUnaryVmOp.Create(AInput: PALFloat);
begin
  inherited Create;
  FInput := AInput;
end;

{************************************************************}
constructor TALExprBinaryVmOp.Create(ALeft, ARight: PALFloat);
begin
  inherited Create;
  FLeft := ALeft;
  FRight := ARight;
end;

{*******************************}
procedure TALExprAddVmOp.Execute;
begin
  FOutput := FLeft^ + FRight^;
end;

{************************************}
procedure TALExprSubtractVmOp.Execute;
begin
  FOutput := FLeft^ - FRight^;
end;

{************************************}
procedure TALExprMultiplyVmOp.Execute;
begin
  FOutput := FLeft^ * FRight^;
end;

{**********************************}
procedure TALExprDivideVmOp.Execute;
begin
  FOutput := FLeft^ / FRight^;
end;

{***********************************}
procedure TALExprCompareVmOp.Execute;
begin
  if FLeft^ < FRight^ then
    FOutput := -1.0
  else
  if FLeft^ > FRight^ then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

{***********************************}
procedure TALExprGreaterVmOp.Execute;
begin
  if FLeft^ > FRight^ then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

{****************************************}
procedure TALExprGreaterEqualVmOp.Execute;
begin
  if FLeft^ >= FRight^ then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

{********************************}
procedure TALExprLessVmOp.Execute;
begin
  if FLeft^ < FRight^ then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

{*************************************}
procedure TALExprLessEqualVmOp.Execute;
begin
  if FLeft^ <= FRight^ then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

{*********************************}
procedure TALExprEqualVmOp.Execute;
begin
  if FLeft^ = FRight^ then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

{************************************}
procedure TALExprNotEqualVmOp.Execute;
begin
  if FLeft^ <> FRight^ then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

{*****************************************}
procedure TALExprIntegerDivideVmOp.Execute;
begin
  FOutput := Round(FLeft^) div Round(FRight^);
end;

{**********************************}
procedure TALExprModuloVmOp.Execute;
begin
  FOutput := Round(FLeft^) mod Round(FRight^);
end;

{*************************************}
procedure TALExprShiftLeftVmOp.Execute;
begin
  FOutput := Round(FLeft^) shl Round(FRight^);
end;

{**************************************}
procedure TALExprShiftRightVmOp.Execute;
begin
  FOutput := Round(FLeft^) shr Round(FRight^);
end;

{**************************************}
procedure TALExprBitwiseAndVmOp.Execute;
begin
  FOutput := Round(FLeft^) and Round(FRight^);
end;

{*************************************}
procedure TALExprBitwiseOrVmOp.Execute;
begin
  FOutput := Round(FLeft^) or Round(FRight^);
end;

{**************************************}
procedure TALExprBitwiseXorVmOp.Execute;
begin
  FOutput := Round(FLeft^) xor Round(FRight^);
end;

{**************************************}
procedure TALExprLogicalAndVmOp.Execute;
begin
  if (FLeft^ <> 0.0) and (FRight^ <> 0) then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

{*************************************}
procedure TALExprLogicalOrVmOp.Execute;
begin
  if (FLeft^ <> 0.0) or (FRight^ <> 0) then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

{**************************************}
procedure TALExprLogicalXorVmOp.Execute;
begin
  if (FLeft^ <> 0.0) xor (FRight^ <> 0) then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

{**********************************}
procedure TALExprNegateVmOp.Execute;
begin
  FOutput := - FInput^;
end;

{**************************************}
procedure TALExprLogicalNotVmOp.Execute;
begin
  if FInput^ <> 0.0 then
    FOutput := 0.0
  else
    FOutput := 1.0;
end;

{**************************************}
procedure TALExprBitwiseNotVmOp.Execute;
begin
  FOutput := not Round(FInput^);
end;

{**************************************************}
constructor TALExprVarVmOp.Create(AVarLoc: Pointer);
begin
  inherited Create;
  FVarLoc := AVarLoc;
end;

{***********************************************************}
constructor TALExprCallFloatVmOp.Create(AFunc: TALFloatFunc);
begin
  inherited Create;
  FFunc := AFunc;
end;

{*************************************}
procedure TALExprCallFloatVmOp.Execute;
begin
  FOutput := FFunc;
end;

{***************************************************************}
constructor TALExprCallFloat32VmOp.Create(AFunc: TALFloat32Func);
begin
  inherited Create;
  FFunc := AFunc;
end;

{***************************************}
procedure TALExprCallFloat32VmOp.Execute;
begin
  FOutput := FFunc;
end;

{***************************************************************}
constructor TALExprCallFloat64VmOp.Create(AFunc: TALFloat64Func);
begin
  inherited Create;
  FFunc := AFunc;
end;

{***************************************}
procedure TALExprCallFloat64VmOp.Execute;
begin
  FOutput := FFunc;
end;

{***************************************************************}
constructor TALExprCallFloat80VmOp.Create(AFunc: TALFloat80Func);
begin
  inherited Create;
  FFunc := AFunc;
end;

{***************************************}
procedure TALExprCallFloat80VmOp.Execute;
begin
  FOutput := FFunc;
end;

{************************************************************************}
constructor TALExprCallUnaryVmOp.Create(AFunc: TALUnaryFunc; X: PALFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
end;

{*************************************}
procedure TALExprCallUnaryVmOp.Execute;
begin
  FOutput := FFunc(FX^);
end;

{****************************************************************************}
constructor TALExprCallUnary32VmOp.Create(AFunc: TALUnary32Func; X: PALFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
end;

{***************************************}
procedure TALExprCallUnary32VmOp.Execute;
begin
  FOutput := FFunc(FX^);
end;

{****************************************************************************}
constructor TALExprCallUnary64VmOp.Create(AFunc: TALUnary64Func; X: PALFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
end;

{***************************************}
procedure TALExprCallUnary64VmOp.Execute;
begin
  FOutput := FFunc(FX^);
end;

{****************************************************************************}
constructor TALExprCallUnary80VmOp.Create(AFunc: TALUnary80Func; X: PALFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
end;

{***************************************}
procedure TALExprCallUnary80VmOp.Execute;
begin
  FOutput := FFunc(FX^);
end;

{*****************************************************************************}
constructor TALExprCallBinaryVmOp.Create(AFunc: TALBinaryFunc; X, Y: PALFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
  FY := Y;
end;

{**************************************}
procedure TALExprCallBinaryVmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^);
end;

{*********************************************************************************}
constructor TALExprCallBinary32VmOp.Create(AFunc: TALBinary32Func; X, Y: PALFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
  FY := Y;
end;

{****************************************}
procedure TALExprCallBinary32VmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^);
end;

{*********************************************************************************}
constructor TALExprCallBinary64VmOp.Create(AFunc: TALBinary64Func; X, Y: PALFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
  FY := Y;
end;

{****************************************}
procedure TALExprCallBinary64VmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^);
end;

{*********************************************************************************}
constructor TALExprCallBinary80VmOp.Create(AFunc: TALBinary80Func; X, Y: PALFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
  FY := Y;
end;

{****************************************}
procedure TALExprCallBinary80VmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^);
end;

{**********************************************************************************}
constructor TALExprCallTernaryVmOp.Create(AFunc: TALTernaryFunc; X, Y, Z: PALFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
  FY := Y;
  FZ := Z;
end;

{***************************************}
procedure TALExprCallTernaryVmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^, FZ^);
end;

{**************************************************************************************}
constructor TALExprCallTernary32VmOp.Create(AFunc: TALTernary32Func; X, Y, Z: PALFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
  FY := Y;
  FZ := Z;
end;

{*****************************************}
procedure TALExprCallTernary32VmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^, FZ^);
end;

{**************************************************************************************}
constructor TALExprCallTernary64VmOp.Create(AFunc: TALTernary64Func; X, Y, Z: PALFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
  FY := Y;
  FZ := Z;
end;

{*****************************************}
procedure TALExprCallTernary64VmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^, FZ^);
end;

{**************************************************************************************}
constructor TALExprCallTernary80VmOp.Create(AFunc: TALTernary80Func; X, Y, Z: PALFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
  FY := Y;
  FZ := Z;
end;

{*****************************************}
procedure TALExprCallTernary80VmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^, FZ^);
end;

{ End of virtual machine operators }

{*********************************}
constructor TALExprVirtMach.Create;
begin
  inherited Create;
  FCodeList := TList.Create;
  FConstList := TList.Create;
end;

{*********************************}
destructor TALExprVirtMach.Destroy;
begin
  ALFreeObjectList(FCodeList);
  ALFreeObjectList(FConstList);
  inherited Destroy;
end;

{*****************************************}
function TALExprVirtMach.Execute: TALFloat;
type
  PExprVirtMachOp = ^TALExprVirtMachOp;
var
  I: Integer;
  pop: PExprVirtMachOp;
begin
  if FCodeList.Count <> 0 then
  begin
    { The code that follows is the same as this, but a lot faster
    for I := 0 to FCodeList.Count - 1 do
      TALExprVirtMachOp(FCodeList[I]).Execute; }
    I := FCodeList.Count;
    pop := @FCodeList.List{$IF CompilerVersion < 23}{Delphi XE2}^{$IFEND}[0];
    while I > 0 do
    begin
      pop^.Execute;
      Inc(pop);
      Dec(I);
    end;
    Result := TALExprVirtMachOp(FCodeList[FCodeList.Count - 1]).FOutput;
  end
  else
    begin
      if (FConstList.Count = 1) then
        Result := TALExprVirtMachOp(FConstList[0]).FOutput
      else
        Result := 0;
    end;
end;

{****************************************************}
procedure TALExprVirtMach.Add(AOp: TALExprVirtMachOp);
begin
  FCodeList.Add(AOp);
end;

{*********************************************************}
procedure TALExprVirtMach.AddConst(AOp: TALExprVirtMachOp);
begin
  FConstList.Add(AOp);
end;

{******************************}
procedure TALExprVirtMach.Clear;
begin
  ALClearObjectList(FCodeList);
  ALClearObjectList(FConstList);
end;

type

  {--------------------------------------}
  TALExprVirtMachNode = class(TALExprNode)
  private
    FExprVmCode: TALExprVirtMachOp;
    function GetVmDeps(AIndex: Integer): TALExprVirtMachNode;
  public
    procedure GenCode(AVirtMach: TALExprVirtMach); virtual; abstract;

    property ExprVmCode: TALExprVirtMachOp read FExprVmCode;

    { this property saves typecasting to access ExprVmCode }
    property VmDeps[AIndex: Integer]: TALExprVirtMachNode read GetVmDeps; default;
  end;

{***************************************************************************}
function TALExprVirtMachNode.GetVmDeps(AIndex: Integer): TALExprVirtMachNode;
begin
  Result := TALExprVirtMachNode(FDepList[AIndex]);
end;

{ Concrete expression nodes for virtual machine }

type

  {---------------------------------------------}
  TALExprUnaryVmNode = class(TALExprVirtMachNode)
  private
    FUnaryClass: TALExprUnaryVmOpClass;
  public
    constructor Create(AUnaryClass: TALExprUnaryVmOpClass;
      const ADeps: array of TALExprNode);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {----------------------------------------------}
  TALExprBinaryVmNode = class(TALExprVirtMachNode)
  private
    FBinaryClass: TALExprBinaryVmOpClass;
  public
    constructor Create(ABinaryClass: TALExprBinaryVmOpClass;
      const ADeps: array of TALExprNode);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {---------------------------------------------}
  TALExprConstVmNode = class(TALExprVirtMachNode)
  private
    FValue: TALFloat;
  public
    constructor Create(AValue: TALFloat);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {---------------------------------------------}
  TALExprVar32VmNode = class(TALExprVirtMachNode)
  private
    FValue: PALFloat32;
  public
    constructor Create(AValue: PALFloat32);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {---------------------------------------------}
  TALExprVar64VmNode = class(TALExprVirtMachNode)
  private
    FValue: PALFloat64;
  public
    constructor Create(AValue: PALFloat64);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {---------------------------------------------}
  TALExprVar80VmNode = class(TALExprVirtMachNode)
  private
    FValue: PALFloat80;
  public
    constructor Create(AValue: PALFloat80);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {-------------------------------------------------}
  TALExprCallFloatVmNode = class(TALExprVirtMachNode)
  private
    FFunc: TALFloatFunc;
  public
    constructor Create(AFunc: TALFloatFunc);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {---------------------------------------------------}
  TALExprCallFloat32VmNode = class(TALExprVirtMachNode)
  private
    FFunc: TALFloat32Func;
  public
    constructor Create(AFunc: TALFloat32Func);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {---------------------------------------------------}
  TALExprCallFloat64VmNode = class(TALExprVirtMachNode)
  private
    FFunc: TALFloat64Func;
  public
    constructor Create(AFunc: TALFloat64Func);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {---------------------------------------------------}
  TALExprCallFloat80VmNode = class(TALExprVirtMachNode)
  private
    FFunc: TALFloat80Func;
  public
    constructor Create(AFunc: TALFloat80Func);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {-------------------------------------------------}
  TALExprCallUnaryVmNode = class(TALExprVirtMachNode)
  private
    FFunc: TALUnaryFunc;
  public
    constructor Create(AFunc: TALUnaryFunc; X: TALExprNode);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {---------------------------------------------------}
  TALExprCallUnary32VmNode = class(TALExprVirtMachNode)
  private
    FFunc: TALUnary32Func;
  public
    constructor Create(AFunc: TALUnary32Func; X: TALExprNode);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {---------------------------------------------------}
  TALExprCallUnary64VmNode = class(TALExprVirtMachNode)
  private
    FFunc: TALUnary64Func;
  public
    constructor Create(AFunc: TALUnary64Func; X: TALExprNode);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {---------------------------------------------------}
  TALExprCallUnary80VmNode = class(TALExprVirtMachNode)
  private
    FFunc: TALUnary80Func;
  public
    constructor Create(AFunc: TALUnary80Func; X: TALExprNode);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {--------------------------------------------------}
  TALExprCallBinaryVmNode = class(TALExprVirtMachNode)
  private
    FFunc: TALBinaryFunc;
  public
    constructor Create(AFunc: TALBinaryFunc; X, Y: TALExprNode);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {----------------------------------------------------}
  TALExprCallBinary32VmNode = class(TALExprVirtMachNode)
  private
    FFunc: TALBinary32Func;
  public
    constructor Create(AFunc: TALBinary32Func; X, Y: TALExprNode);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {----------------------------------------------------}
  TALExprCallBinary64VmNode = class(TALExprVirtMachNode)
  private
    FFunc: TALBinary64Func;
  public
    constructor Create(AFunc: TALBinary64Func; X, Y: TALExprNode);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {----------------------------------------------------}
  TALExprCallBinary80VmNode = class(TALExprVirtMachNode)
  private
    FFunc: TALBinary80Func;
  public
    constructor Create(AFunc: TALBinary80Func; X, Y: TALExprNode);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {---------------------------------------------------}
  TALExprCallTernaryVmNode = class(TALExprVirtMachNode)
  private
    FFunc: TALTernaryFunc;
  public
    constructor Create(AFunc: TALTernaryFunc; X, Y, Z: TALExprNode);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {-----------------------------------------------------}
  TALExprCallTernary32VmNode = class(TALExprVirtMachNode)
  private
    FFunc: TALTernary32Func;
  public
    constructor Create(AFunc: TALTernary32Func; X, Y, Z: TALExprNode);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {-----------------------------------------------------}
  TALExprCallTernary64VmNode = class(TALExprVirtMachNode)
  private
    FFunc: TALTernary64Func;
  public
    constructor Create(AFunc: TALTernary64Func; X, Y, Z: TALExprNode);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

  {-----------------------------------------------------}
  TALExprCallTernary80VmNode = class(TALExprVirtMachNode)
  private
    FFunc: TALTernary80Func;
  public
    constructor Create(AFunc: TALTernary80Func; X, Y, Z: TALExprNode);
    procedure GenCode(AVirtMach: TALExprVirtMach); override;
  end;

{***********************************************************************************************************}
constructor TALExprUnaryVmNode.Create(AUnaryClass: TALExprUnaryVmOpClass; const ADeps: array of TALExprNode);
begin
  FUnaryClass := AUnaryClass;
  inherited Create(ADeps);
  Assert(FDepList.Count = 1);
end;

{***************************************************************}
procedure TALExprUnaryVmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := FUnaryClass.Create(VmDeps[0].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

{**************************************************************************************************************}
constructor TALExprBinaryVmNode.Create(ABinaryClass: TALExprBinaryVmOpClass; const ADeps: array of TALExprNode);
begin
  FBinaryClass := ABinaryClass;
  inherited Create(ADeps);
  Assert(FDepList.Count = 2);
end;

{****************************************************************}
procedure TALExprBinaryVmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := FBinaryClass.Create(
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

{******************************************************}
constructor TALExprConstVmNode.Create(AValue: TALFloat);
begin
  FValue := AValue;
  inherited Create([]);
end;

{***************************************************************}
procedure TALExprConstVmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprConstVmOp.Create(FValue);
  AVirtMach.AddConst(FExprVmCode);
end;

{********************************************************}
constructor TALExprVar32VmNode.Create(AValue: PALFloat32);
begin
  FValue := AValue;
  inherited Create([]);
end;

{***************************************************************}
procedure TALExprVar32VmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprVar32VmOp.Create(FValue);
  AVirtMach.Add(FExprVmCode);
end;

{********************************************************}
constructor TALExprVar64VmNode.Create(AValue: PALFloat64);
begin
  FValue := AValue;
  inherited Create([]);
end;

{***************************************************************}
procedure TALExprVar64VmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprVar64VmOp.Create(FValue);
  AVirtMach.Add(FExprVmCode);
end;

{********************************************************}
constructor TALExprVar80VmNode.Create(AValue: PALFloat80);
begin
  FValue := AValue;
  inherited Create([]);
end;

{***************************************************************}
procedure TALExprVar80VmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprVar80VmOp.Create(FValue);
  AVirtMach.Add(FExprVmCode);
end;

{ End of expression nodes for virtual machine }

{********************************************}
constructor TALExprVirtMachNodeFactory.Create;
begin
  inherited Create;
  FNodeList := TList.Create;
end;

{********************************************}
destructor TALExprVirtMachNodeFactory.Destroy;
begin
  ALFreeObjectList(FNodeList);
  inherited Destroy;
end;

{***************************************************************************}
function TALExprVirtMachNodeFactory.AddNode(ANode: TALExprNode): TALExprNode;
begin
  Result := ANode;
  FNodeList.Add(ANode);
end;

{***********************************************************************}
procedure TALExprVirtMachNodeFactory.GenCode(AVirtMach: TALExprVirtMach);
begin
  { TODO : optimize the expression tree into a DAG (i.e. find CSEs) and
    evaluate constant subexpressions, implement strength reduction, etc. }

  { TODO : move optimization logic (as far as possible) into ancestor classes
    once tested and interfaces are solid, so that other evaluation strategies
    can take advantage of these optimizations. }

  DoClean(AVirtMach);
  DoConsts(AVirtMach);
  DoCode(AVirtMach);
end;

{***************************************************************************}
function TALExprVirtMachNodeFactory.LoadVar32(ALoc: PALFloat32): TALExprNode;
begin
  Result := AddNode(TALExprVar32VmNode.Create(ALoc));
end;

{***************************************************************************}
function TALExprVirtMachNodeFactory.LoadVar64(ALoc: PALFloat64): TALExprNode;
begin
  Result := AddNode(TALExprVar64VmNode.Create(ALoc));
end;

{***************************************************************************}
function TALExprVirtMachNodeFactory.LoadVar80(ALoc: PALFloat80): TALExprNode;
begin
  Result := AddNode(TALExprVar80VmNode.Create(ALoc));
end;

{*******************************************************************************}
function TALExprVirtMachNodeFactory.LoadConst32(AValue: TALFloat32): TALExprNode;
begin
  Result := AddNode(TALExprConstVmNode.Create(AValue));
end;

{*******************************************************************************}
function TALExprVirtMachNodeFactory.LoadConst64(AValue: TALFloat64): TALExprNode;
begin
  Result := AddNode(TALExprConstVmNode.Create(AValue));
end;

{*******************************************************************************}
function TALExprVirtMachNodeFactory.LoadConst80(AValue: TALFloat80): TALExprNode;
begin
  Result := AddNode(TALExprConstVmNode.Create(AValue));
end;

{*******************************************************************************}
function TALExprVirtMachNodeFactory.Add(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprAddVmOp, [ALeft, ARight]));
end;

{************************************************************************************}
function TALExprVirtMachNodeFactory.Subtract(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprSubtractVmOp, [ALeft, ARight]));
end;

{************************************************************************************}
function TALExprVirtMachNodeFactory.Multiply(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprMultiplyVmOp, [ALeft, ARight]));
end;

{**********************************************************************************}
function TALExprVirtMachNodeFactory.Divide(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprDivideVmOp, [ALeft, ARight]));
end;

{*****************************************************************************************}
function TALExprVirtMachNodeFactory.IntegerDivide(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprIntegerDivideVmOp, [ALeft, ARight]));
end;

{**********************************************************************************}
function TALExprVirtMachNodeFactory.Modulo(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprModuloVmOp, [ALeft, ARight]));
end;

{***************************************************************************}
function TALExprVirtMachNodeFactory.Negate(AValue: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprUnaryVmNode.Create(TALExprNegateVmOp, [AValue]));
end;

{***********************************************************************}
procedure TALExprVirtMachNodeFactory.DoClean(AVirtMach: TALExprVirtMach);
var
  I: Integer;
begin
  { clean up in preparation for code generation }
  AVirtMach.Clear;
  for I := 0 to FNodeList.Count - 1 do
    TALExprVirtMachNode(FNodeList[I]).FExprVmCode := nil;
end;

{************************************************************************}
procedure TALExprVirtMachNodeFactory.DoConsts(AVirtMach: TALExprVirtMach);
var
  I: Integer;
  Node: TALExprVirtMachNode;
begin
  { process consts }
  for I := 0 to FNodeList.Count - 1 do
  begin
    Node := TALExprVirtMachNode(FNodeList[I]);
    if (Node is TALExprConstVmNode) and (Node.ExprVmCode = nil) then
      Node.GenCode(AVirtMach);
  end;
end;

{**********************************************************************}
procedure TALExprVirtMachNodeFactory.DoCode(AVirtMach: TALExprVirtMach);
var
  I: Integer;
  Node: TALExprVirtMachNode;
begin
  { process code }
  for I := 0 to FNodeList.Count - 1 do
  begin
    Node := TALExprVirtMachNode(FNodeList[I]);
    if Node.ExprVmCode = nil then
      Node.GenCode(AVirtMach);
  end;
end;

{**********************************************************************************}
function TALExprVirtMachNodeFactory.CallFloatFunc(AFunc: TALFloatFunc): TALExprNode;
begin
  Result := AddNode(TALExprCallFloatVmNode.Create(AFunc));
end;

{**************************************************************************************}
function TALExprVirtMachNodeFactory.CallFloat32Func(AFunc: TALFloat32Func): TALExprNode;
begin
  Result := AddNode(TALExprCallFloat32VmNode.Create(AFunc));
end;

{**************************************************************************************}
function TALExprVirtMachNodeFactory.CallFloat64Func(AFunc: TALFloat64Func): TALExprNode;
begin
  Result := AddNode(TALExprCallFloat64VmNode.Create(AFunc));
end;

{**************************************************************************************}
function TALExprVirtMachNodeFactory.CallFloat80Func(AFunc: TALFloat80Func): TALExprNode;
begin
  Result := AddNode(TALExprCallFloat80VmNode.Create(AFunc));
end;

{**************************************************************************************************}
function TALExprVirtMachNodeFactory.CallUnaryFunc(AFunc: TALUnaryFunc; X: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprCallUnaryVmNode.Create(AFunc, X));
end;

{******************************************************************************************************}
function TALExprVirtMachNodeFactory.CallUnary32Func(AFunc: TALUnary32Func; X: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprCallUnary32VmNode.Create(AFunc, X));
end;

{******************************************************************************************************}
function TALExprVirtMachNodeFactory.CallUnary64Func(AFunc: TALUnary64Func; X: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprCallUnary64VmNode.Create(AFunc, X));
end;

{******************************************************************************************************}
function TALExprVirtMachNodeFactory.CallUnary80Func(AFunc: TALUnary80Func; X: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprCallUnary80VmNode.Create(AFunc, X));
end;

{*******************************************************************************************************}
function TALExprVirtMachNodeFactory.CallBinaryFunc(AFunc: TALBinaryFunc; X, Y: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprCallBinaryVmNode.Create(AFunc, X, Y));
end;

{***********************************************************************************************************}
function TALExprVirtMachNodeFactory.CallBinary32Func(AFunc: TALBinary32Func; X, Y: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprCallBinary32VmNode.Create(AFunc, X, Y));
end;

{***********************************************************************************************************}
function TALExprVirtMachNodeFactory.CallBinary64Func(AFunc: TALBinary64Func; X, Y: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprCallBinary64VmNode.Create(AFunc, X, Y));
end;

{***********************************************************************************************************}
function TALExprVirtMachNodeFactory.CallBinary80Func(AFunc: TALBinary80Func; X, Y: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprCallBinary80VmNode.Create(AFunc, X, Y));
end;

{************************************************************************************************************}
function TALExprVirtMachNodeFactory.CallTernaryFunc(AFunc: TALTernaryFunc; X, Y, Z: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprCallTernaryVmNode.Create(AFunc, X, Y, Z));
end;

{****************************************************************************************************************}
function TALExprVirtMachNodeFactory.CallTernary32Func(AFunc: TALTernary32Func; X, Y, Z: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprCallTernary32VmNode.Create(AFunc, X, Y, Z));
end;

{****************************************************************************************************************}
function TALExprVirtMachNodeFactory.CallTernary64Func(AFunc: TALTernary64Func; X, Y, Z: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprCallTernary64VmNode.Create(AFunc, X, Y, Z));
end;

{****************************************************************************************************************}
function TALExprVirtMachNodeFactory.CallTernary80Func(AFunc: TALTernary80Func; X, Y, Z: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprCallTernary80VmNode.Create(AFunc, X, Y, Z));
end;

{***********************************************************************************}
function TALExprVirtMachNodeFactory.Compare(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprCompareVmOp, [ALeft, ARight]));
end;

{****************************************************************************************}
function TALExprVirtMachNodeFactory.CompareEqual(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprEqualVmOp, [ALeft, ARight]));
end;

{*******************************************************************************************}
function TALExprVirtMachNodeFactory.CompareNotEqual(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprNotEqualVmOp, [ALeft, ARight]));
end;

{***************************************************************************************}
function TALExprVirtMachNodeFactory.CompareLess(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprLessVmOp, [ALeft, ARight]));
end;

{********************************************************************************************}
function TALExprVirtMachNodeFactory.CompareLessEqual(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprLessEqualVmOp, [ALeft, ARight]));
end;

{******************************************************************************************}
function TALExprVirtMachNodeFactory.CompareGreater(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprGreaterVmOp, [ALeft, ARight]));
end;

{***********************************************************************************************}
function TALExprVirtMachNodeFactory.CompareGreaterEqual(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprGreaterEqualVmOp, [ALeft, ARight]));
end;

{**************************************************************************************}
function TALExprVirtMachNodeFactory.LogicalAnd(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprLogicalAndVmOp, [ALeft, ARight]));
end;

{*************************************************************************************}
function TALExprVirtMachNodeFactory.LogicalOr(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprLogicalOrVmOp, [ALeft, ARight]));
end;

{**************************************************************************************}
function TALExprVirtMachNodeFactory.LogicalXor(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprLogicalXorVmOp, [ALeft, ARight]));
end;

{*******************************************************************************}
function TALExprVirtMachNodeFactory.LogicalNot(AValue: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprUnaryVmNode.Create(TALExprLogicalNotVmOp, [AValue]));
end;

{**************************************************************************************}
function TALExprVirtMachNodeFactory.BitwiseAnd(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprBitwiseAndVmOp, [ALeft, ARight]));
end;

{*************************************************************************************}
function TALExprVirtMachNodeFactory.BitwiseOr(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprBitwiseOrVmOp, [ALeft, ARight]));
end;

{**************************************************************************************}
function TALExprVirtMachNodeFactory.BitwiseXor(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprBitwiseXorVmOp, [ALeft, ARight]));
end;

{*******************************************************************************}
function TALExprVirtMachNodeFactory.BitwiseNot(AValue: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprUnaryVmNode.Create(TALExprBitwiseNotVmOp, [AValue]));
end;

{*************************************************************************************}
function TALExprVirtMachNodeFactory.ShiftLeft(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprShiftLeftVmOp, [ALeft, ARight]));
end;

{**************************************************************************************}
function TALExprVirtMachNodeFactory.ShiftRight(ALeft, ARight: TALExprNode): TALExprNode;
begin
  Result := AddNode(TALExprBinaryVmNode.Create(TALExprShiftRightVmOp, [ALeft, ARight]));
end;

{**************************************}
constructor TALCompiledEvaluator.Create;
begin
  inherited Create;
  FVirtMach := TALExprVirtMach.Create;
end;

{**************************************}
destructor TALCompiledEvaluator.Destroy;
begin
  FVirtMach.Free;
  inherited Destroy;
end;

{**************************************************************}
procedure TALCompiledEvaluator.Compile(const AExpr: AnsiString);
var
  Lex: TALExprSimpleLexer;
  Parse: TALExprCompileParser;
  NodeFactory: TALExprVirtMachNodeFactory;
begin
  if AExpr <> FExpr then
  begin
    FExpr := AExpr;
    FVirtMach.Clear;

    Parse := nil;
    NodeFactory := nil;
    Lex := TALExprSimpleLexer.Create(FExpr);
    try
      NodeFactory := TALExprVirtMachNodeFactory.Create;
      Parse := TALExprCompileParser.Create(Lex, NodeFactory);
      Parse.Context := InternalContextSet;
      Parse.Compile;
      NodeFactory.GenCode(FVirtMach);
    finally
      Parse.Free;
      NodeFactory.Free;
      Lex.Free;
    end;
  end;
end;

{***********************************************}
function TALCompiledEvaluator.Evaluate: TALFloat;
begin
  Result := FVirtMach.Execute;
end;

{*****************************************************************************}
constructor TALExprVar32Sym.Create(const AIdent: AnsiString; ALoc: PALFloat32);
begin
  Assert(ALoc <> nil);
  FLoc := ALoc;
  inherited Create(AIdent);
end;

{********************************************}
function TALExprVar32Sym.Compile: TALExprNode;
begin
  Result := NodeFactory.LoadVar32(FLoc);
end;

{******************************************}
function TALExprVar32Sym.Evaluate: TALFloat;
begin
  Result := FLoc^;
end;

{*****************************************************************************}
constructor TALExprVar64Sym.Create(const AIdent: AnsiString; ALoc: PALFloat64);
begin
  Assert(ALoc <> nil);
  FLoc := ALoc;
  inherited Create(AIdent);
end;

{********************************************}
function TALExprVar64Sym.Compile: TALExprNode;
begin
  Result := NodeFactory.LoadVar64(FLoc);
end;

{******************************************}
function TALExprVar64Sym.Evaluate: TALFloat;
begin
  Result := FLoc^;
end;

{*****************************************************************************}
constructor TALExprVar80Sym.Create(const AIdent: AnsiString; ALoc: PALFloat80);
begin
  Assert(ALoc <> nil);
  FLoc := ALoc;
  inherited Create(AIdent);
end;

{********************************************}
function TALExprVar80Sym.Compile: TALExprNode;
begin
  Result := NodeFactory.LoadVar80(FLoc);
end;

{******************************************}
function TALExprVar80Sym.Evaluate: TALFloat;
begin
  Result := FLoc^;
end;

{*************************************************************}
constructor TALExprCallFloatVmNode.Create(AFunc: TALFloatFunc);
begin
  FFunc := AFunc;
  inherited Create([]);
end;

{*******************************************************************}
procedure TALExprCallFloatVmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprCallFloatVmOp.Create(FFunc);
  AVirtMach.Add(FExprVmCode);
end;

{*****************************************************************}
constructor TALExprCallFloat32VmNode.Create(AFunc: TALFloat32Func);
begin
  FFunc := AFunc;
  inherited Create([]);
end;

{*********************************************************************}
procedure TALExprCallFloat32VmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprCallFloat32VmOp.Create(FFunc);
  AVirtMach.Add(FExprVmCode);
end;

{*****************************************************************}
constructor TALExprCallFloat64VmNode.Create(AFunc: TALFloat64Func);
begin
  FFunc := AFunc;
  inherited Create([]);
end;

{*********************************************************************}
procedure TALExprCallFloat64VmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprCallFloat64VmOp.Create(FFunc);
  AVirtMach.Add(FExprVmCode);
end;

{*****************************************************************}
constructor TALExprCallFloat80VmNode.Create(AFunc: TALFloat80Func);
begin
  FFunc := AFunc;
  inherited Create([]);
end;

{*********************************************************************}
procedure TALExprCallFloat80VmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprCallFloat80VmOp.Create(FFunc);
  AVirtMach.Add(FExprVmCode);
end;

{*****************************************************************************}
constructor TALExprCallUnaryVmNode.Create(AFunc: TALUnaryFunc; X: TALExprNode);
begin
  FFunc := AFunc;
  inherited Create([X]);
end;

{*******************************************************************}
procedure TALExprCallUnaryVmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprCallUnaryVmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

{*********************************************************************************}
constructor TALExprCallUnary32VmNode.Create(AFunc: TALUnary32Func; X: TALExprNode);
begin
  FFunc := AFunc;
  inherited Create([X]);
end;

{*********************************************************************}
procedure TALExprCallUnary32VmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprCallUnary32VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

{*********************************************************************************}
constructor TALExprCallUnary64VmNode.Create(AFunc: TALUnary64Func; X: TALExprNode);
begin
  FFunc := AFunc;
  inherited Create([X]);
end;

{*********************************************************************}
procedure TALExprCallUnary64VmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprCallUnary64VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

{*********************************************************************************}
constructor TALExprCallUnary80VmNode.Create(AFunc: TALUnary80Func; X: TALExprNode);
begin
  FFunc := AFunc;
  inherited Create([X]);
end;

{*********************************************************************}
procedure TALExprCallUnary80VmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprCallUnary80VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

{**********************************************************************************}
constructor TALExprCallBinaryVmNode.Create(AFunc: TALBinaryFunc; X, Y: TALExprNode);
begin
  FFunc := AFunc;
  inherited Create([X, Y]);
end;

{********************************************************************}
procedure TALExprCallBinaryVmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprCallBinaryVmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

{**************************************************************************************}
constructor TALExprCallBinary32VmNode.Create(AFunc: TALBinary32Func; X, Y: TALExprNode);
begin
  FFunc := AFunc;
  inherited Create([X, Y]);
end;

{**********************************************************************}
procedure TALExprCallBinary32VmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprCallBinary32VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

{**************************************************************************************}
constructor TALExprCallBinary64VmNode.Create(AFunc: TALBinary64Func; X, Y: TALExprNode);
begin
  FFunc := AFunc;
  inherited Create([X, Y]);
end;

{**********************************************************************}
procedure TALExprCallBinary64VmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprCallBinary64VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

{**************************************************************************************}
constructor TALExprCallBinary80VmNode.Create(AFunc: TALBinary80Func; X, Y: TALExprNode);
begin
  FFunc := AFunc;
  inherited Create([X, Y]);
end;

{**********************************************************************}
procedure TALExprCallBinary80VmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprCallBinary80VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

{***************************************************************************************}
constructor TALExprCallTernaryVmNode.Create(AFunc: TALTernaryFunc; X, Y, Z: TALExprNode);
begin
  FFunc := AFunc;
  inherited Create([X, Y, Z]);
end;

{*********************************************************************}
procedure TALExprCallTernaryVmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprCallTernaryVmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc,
    VmDeps[2].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

{*******************************************************************************************}
constructor TALExprCallTernary32VmNode.Create(AFunc: TALTernary32Func; X, Y, Z: TALExprNode);
begin
  FFunc := AFunc;
  inherited Create([X, Y, Z]);
end;

{***********************************************************************}
procedure TALExprCallTernary32VmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprCallTernary32VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc,
    VmDeps[2].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

{*******************************************************************************************}
constructor TALExprCallTernary64VmNode.Create(AFunc: TALTernary64Func; X, Y, Z: TALExprNode);
begin
  FFunc := AFunc;
  inherited Create([X, Y, Z]);
end;

{***********************************************************************}
procedure TALExprCallTernary64VmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprCallTernary64VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc,
    VmDeps[2].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

{*******************************************************************************************}
constructor TALExprCallTernary80VmNode.Create(AFunc: TALTernary80Func; X, Y, Z: TALExprNode);
begin
  FFunc := AFunc;
  inherited Create([X, Y, Z]);
end;

{***********************************************************************}
procedure TALExprCallTernary80VmNode.GenCode(AVirtMach: TALExprVirtMach);
begin
  FExprVmCode := TALExprCallTernary80VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc,
    VmDeps[2].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

{***********************************************************}
function TALExprAbstractFuncSym.CompileFirstArg: TALExprNode;
begin
  if Lexer.CurrTok <> etLParen then
    raise EALExprEvalError.CreateRes(@RsALExprEvalFirstArg);
  Result := CompileParser.CompileExprLevel0(True);
end;

{**********************************************************}
function TALExprAbstractFuncSym.CompileNextArg: TALExprNode;
begin
  if Lexer.CurrTok <> etComma then
    raise EALExprEvalError.CreateRes(@RsALExprEvalNextArg);
  Result := CompileParser.CompileExprLevel0(True);
end;

{*****************************************************}
function TALExprAbstractFuncSym.EvalFirstArg: TALFloat;
begin
  if Lexer.CurrTok <> etLParen then
    raise EALExprEvalError.CreateRes(@RsALExprEvalFirstArg);
  Result := EvalParser.EvalExprLevel0(True);
end;

{****************************************************}
function TALExprAbstractFuncSym.EvalNextArg: TALFloat;
begin
  if Lexer.CurrTok <> etComma then
    raise EALExprEvalError.CreateRes(@RsALExprEvalNextArg);
  Result := EvalParser.EvalExprLevel0(True);
end;

{***************************************}
procedure TALExprAbstractFuncSym.EndArgs;
begin
  if Lexer.CurrTok <> etRParen then
    raise EALExprEvalError.CreateRes(@RsALExprEvalEndArgs);
  Lexer.NextTok;
end;

{*******************************************************************************}
constructor TALExprFuncSym.Create(const AIdent: AnsiString; AFunc: TALFloatFunc);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

{*******************************************}
function TALExprFuncSym.Compile: TALExprNode;
begin
  Result := NodeFactory.CallFloatFunc(FFunc);
end;

{*****************************************}
function TALExprFuncSym.Evaluate: TALFloat;
begin
  Result := FFunc;
end;

{****************************************************************************************}
constructor TALExprFloat32FuncSym.Create(const AIdent: AnsiString; AFunc: TALFloat32Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

{**************************************************}
function TALExprFloat32FuncSym.Compile: TALExprNode;
begin
  Result := NodeFactory.CallFloat32Func(FFunc);
end;

{************************************************}
function TALExprFloat32FuncSym.Evaluate: TALFloat;
begin
  Result := FFunc;
end;

{****************************************************************************************}
constructor TALExprFloat64FuncSym.Create(const AIdent: AnsiString; AFunc: TALFloat64Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

{**************************************************}
function TALExprFloat64FuncSym.Compile: TALExprNode;
begin
  Result := NodeFactory.CallFloat64Func(FFunc);
end;

{************************************************}
function TALExprFloat64FuncSym.Evaluate: TALFloat;
begin
  Result := FFunc;
end;

{****************************************************************************************}
constructor TALExprFloat80FuncSym.Create(const AIdent: AnsiString; AFunc: TALFloat80Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

{**************************************************}
function TALExprFloat80FuncSym.Compile: TALExprNode;
begin
  Result := NodeFactory.CallFloat80Func(FFunc);
end;

{************************************************}
function TALExprFloat80FuncSym.Evaluate: TALFloat;
begin
  Result := FFunc;
end;

{************************************************************************************}
constructor TALExprUnaryFuncSym.Create(const AIdent: AnsiString; AFunc: TALUnaryFunc);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

{************************************************}
function TALExprUnaryFuncSym.Compile: TALExprNode;
var
  X: TALExprNode;
begin
  X := CompileFirstArg;
  EndArgs;
  Result := NodeFactory.CallUnaryFunc(FFunc, X);
end;

{**********************************************}
function TALExprUnaryFuncSym.Evaluate: TALFloat;
var
  X: TALFloat;
begin
  X := EvalFirstArg;
  EndArgs;
  Result := FFunc(X);
end;

{****************************************************************************************}
constructor TALExprUnary32FuncSym.Create(const AIdent: AnsiString; AFunc: TALUnary32Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

{**************************************************}
function TALExprUnary32FuncSym.Compile: TALExprNode;
var
  X: TALExprNode;
begin
  X := CompileFirstArg;
  EndArgs;
  Result := NodeFactory.CallUnary32Func(FFunc, X);
end;

{************************************************}
function TALExprUnary32FuncSym.Evaluate: TALFloat;
var
  X: TALFloat;
begin
  X := EvalFirstArg;
  EndArgs;
  Result := FFunc(X);
end;

{****************************************************************************************}
constructor TALExprUnary64FuncSym.Create(const AIdent: AnsiString; AFunc: TALUnary64Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

{**************************************************}
function TALExprUnary64FuncSym.Compile: TALExprNode;
var
  X: TALExprNode;
begin
  X := CompileFirstArg;
  EndArgs;
  Result := NodeFactory.CallUnary64Func(FFunc, X);
end;

{************************************************}
function TALExprUnary64FuncSym.Evaluate: TALFloat;
var
  X: TALFloat;
begin
  X := EvalFirstArg;
  EndArgs;
  Result := FFunc(X);
end;

{****************************************************************************************}
constructor TALExprUnary80FuncSym.Create(const AIdent: AnsiString; AFunc: TALUnary80Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

{**************************************************}
function TALExprUnary80FuncSym.Compile: TALExprNode;
var
  X: TALExprNode;
begin
  X := CompileFirstArg;
  EndArgs;
  Result := NodeFactory.CallUnary80Func(FFunc, X);
end;

{************************************************}
function TALExprUnary80FuncSym.Evaluate: TALFloat;
var
  X: TALFloat;
begin
  X := EvalFirstArg;
  EndArgs;
  Result := FFunc(X);
end;

{**************************************************************************************}
constructor TALExprBinaryFuncSym.Create(const AIdent: AnsiString; AFunc: TALBinaryFunc);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

{*************************************************}
function TALExprBinaryFuncSym.Compile: TALExprNode;
var
  X, Y: TALExprNode;
begin
  // must be called this way, because evaluation order of function
  // parameters is not defined; we need CompileFirstArg to be called
  // first.
  X := CompileFirstArg;
  Y := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallBinaryFunc(FFunc, X, Y);
end;

{***********************************************}
function TALExprBinaryFuncSym.Evaluate: TALFloat;
var
  X, Y: TALFloat;
begin
  X := EvalFirstArg;
  Y := EvalNextArg;
  EndArgs;
  Result := FFunc(X, Y);
end;

{******************************************************************************************}
constructor TALExprBinary32FuncSym.Create(const AIdent: AnsiString; AFunc: TALBinary32Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

{***************************************************}
function TALExprBinary32FuncSym.Compile: TALExprNode;
var
  X, Y: TALExprNode;
begin
  X := CompileFirstArg;
  Y := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallBinary32Func(FFunc, X, Y);
end;

{*************************************************}
function TALExprBinary32FuncSym.Evaluate: TALFloat;
var
  X, Y: TALFloat;
begin
  X := EvalFirstArg;
  Y := EvalNextArg;
  EndArgs;
  Result := FFunc(X, Y);
end;

{******************************************************************************************}
constructor TALExprBinary64FuncSym.Create(const AIdent: AnsiString; AFunc: TALBinary64Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

{***************************************************}
function TALExprBinary64FuncSym.Compile: TALExprNode;
var
  X, Y: TALExprNode;
begin
  X := CompileFirstArg;
  Y := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallBinary64Func(FFunc, X, Y);
end;

{*************************************************}
function TALExprBinary64FuncSym.Evaluate: TALFloat;
var
  X, Y: TALFloat;
begin
  X := EvalFirstArg;
  Y := EvalNextArg;
  EndArgs;
  Result := FFunc(X, Y);
end;

{******************************************************************************************}
constructor TALExprBinary80FuncSym.Create(const AIdent: AnsiString; AFunc: TALBinary80Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

{***************************************************}
function TALExprBinary80FuncSym.Compile: TALExprNode;
var
  X, Y: TALExprNode;
begin
  X := CompileFirstArg;
  Y := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallBinary80Func(FFunc, X, Y);
end;

{*************************************************}
function TALExprBinary80FuncSym.Evaluate: TALFloat;
var
  X, Y: TALFloat;
begin
  X := EvalFirstArg;
  Y := EvalNextArg;
  EndArgs;
  Result := FFunc(X, Y);
end;

{****************************************************************************************}
constructor TALExprTernaryFuncSym.Create(const AIdent: AnsiString; AFunc: TALTernaryFunc);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

{**************************************************}
function TALExprTernaryFuncSym.Compile: TALExprNode;
var
  X, Y, Z: TALExprNode;
begin
  X := CompileFirstArg;
  Y := CompileNextArg;
  Z := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallTernaryFunc(FFunc, X, Y, Z);
end;

{************************************************}
function TALExprTernaryFuncSym.Evaluate: TALFloat;
var
  X, Y, Z: TALFloat;
begin
  X := EvalFirstArg;
  Y := EvalNextArg;
  Z := EvalNextArg;
  EndArgs;
  Result := FFunc(X, Y, Z);
end;

{********************************************************************************************}
constructor TALExprTernary32FuncSym.Create(const AIdent: AnsiString; AFunc: TALTernary32Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

{****************************************************}
function TALExprTernary32FuncSym.Compile: TALExprNode;
var
  X, Y, Z: TALExprNode;
begin
  X := CompileFirstArg;
  Y := CompileNextArg;
  Z := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallTernary32Func(FFunc, X, Y, Z);
end;

{**************************************************}
function TALExprTernary32FuncSym.Evaluate: TALFloat;
var
  X, Y, Z: TALFloat;
begin
  X := EvalFirstArg;
  Y := EvalNextArg;
  Z := EvalNextArg;
  EndArgs;
  Result := FFunc(X, Y, Z);
end;

{********************************************************************************************}
constructor TALExprTernary64FuncSym.Create(const AIdent: AnsiString; AFunc: TALTernary64Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

{****************************************************}
function TALExprTernary64FuncSym.Compile: TALExprNode;
var
  X, Y, Z: TALExprNode;
begin
  X := CompileFirstArg;
  Y := CompileNextArg;
  Z := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallTernary64Func(FFunc, X, Y, Z);
end;

{**************************************************}
function TALExprTernary64FuncSym.Evaluate: TALFloat;
var
  X, Y, Z: TALFloat;
begin
  X := EvalFirstArg;
  Y := EvalNextArg;
  Z := EvalNextArg;
  EndArgs;
  Result := FFunc(X, Y, Z);
end;

{********************************************************************************************}
constructor TALExprTernary80FuncSym.Create(const AIdent: AnsiString; AFunc: TALTernary80Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

{****************************************************}
function TALExprTernary80FuncSym.Compile: TALExprNode;
var
  X, Y, Z: TALExprNode;
begin
  X := CompileFirstArg;
  Y := CompileNextArg;
  Z := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallTernary80Func(FFunc, X, Y, Z);
end;

{**************************************************}
function TALExprTernary80FuncSym.Evaluate: TALFloat;
var
  X, Y, Z: TALFloat;
begin
  X := EvalFirstArg;
  Y := EvalNextArg;
  Z := EvalNextArg;
  EndArgs;
  Result := FFunc(X, Y, Z);
end;

{*****************************************************************************}
constructor TALExprConstSym.Create(const AIdent: AnsiString; AValue: TALFloat);
begin
  inherited Create(AIdent);
  FValue := AValue;
end;

{********************************************}
function TALExprConstSym.Compile: TALExprNode;
begin
  Result := NodeFactory.LoadConst(FValue);
end;

{******************************************}
function TALExprConstSym.Evaluate: TALFloat;
begin
  Result := FValue;
end;

{*********************************************************************************}
constructor TALExprConst32Sym.Create(const AIdent: AnsiString; AValue: TALFloat32);
begin
  inherited Create(AIdent);
  FValue := AValue;
end;

{**********************************************}
function TALExprConst32Sym.Compile: TALExprNode;
begin
  Result := NodeFactory.LoadConst(FValue);
end;

{********************************************}
function TALExprConst32Sym.Evaluate: TALFloat;
begin
  Result := FValue;
end;

{*********************************************************************************}
constructor TALExprConst64Sym.Create(const AIdent: AnsiString; AValue: TALFloat64);
begin
  inherited Create(AIdent);
  FValue := AValue;
end;

{**********************************************}
function TALExprConst64Sym.Compile: TALExprNode;
begin
  Result := NodeFactory.LoadConst(FValue);
end;

{********************************************}
function TALExprConst64Sym.Evaluate: TALFloat;
begin
  Result := FValue;
end;

{*********************************************************************************}
constructor TALExprConst80Sym.Create(const AIdent: AnsiString; AValue: TALFloat80);
begin
  inherited Create(AIdent);
  FValue := AValue;
end;

{**********************************************}
function TALExprConst80Sym.Compile: TALExprNode;
begin
  Result := NodeFactory.LoadConst(FValue);
end;

{********************************************}
function TALExprConst80Sym.Evaluate: TALFloat;
begin
  Result := FValue;
end;

{**********************************}
constructor TALEasyEvaluator.Create;
begin
  inherited Create;
  FOwnContext := TALExprHashContext.Create(False);
  FExtContextSet := TALExprSetContext.Create(False);
  FInternalContextSet := TALExprSetContext.Create(False);

  // user added names get precedence over external context's names
  FInternalContextSet.Add(FExtContextSet);
  FInternalContextSet.Add(FOwnContext);
end;

{**********************************}
destructor TALEasyEvaluator.Destroy;
begin
  FInternalContextSet.Free;
  FOwnContext.Free;
  FExtContextSet.Free;
  inherited Destroy;
end;

{*******************************************************************************}
procedure TALEasyEvaluator.AddConst(const AName: AnsiString; AConst: TALFloat80);
begin
  FOwnContext.Add(TALExprConst80Sym.Create(AName, AConst));
end;

{*******************************************************************************}
procedure TALEasyEvaluator.AddConst(const AName: AnsiString; AConst: TALFloat64);
begin
  FOwnContext.Add(TALExprConst64Sym.Create(AName, AConst));
end;

{*******************************************************************************}
procedure TALEasyEvaluator.AddConst(const AName: AnsiString; AConst: TALFloat32);
begin
  FOwnContext.Add(TALExprConst32Sym.Create(AName, AConst));
end;

{*******************************************************************************}
procedure TALEasyEvaluator.AddVar(const AName: AnsiString; var AVar: TALFloat32);
begin
  FOwnContext.Add(TALExprVar32Sym.Create(AName, @AVar));
end;

{*******************************************************************************}
procedure TALEasyEvaluator.AddVar(const AName: AnsiString; var AVar: TALFloat64);
begin
  FOwnContext.Add(TALExprVar64Sym.Create(AName, @AVar));
end;

{*******************************************************************************}
procedure TALEasyEvaluator.AddVar(const AName: AnsiString; var AVar: TALFloat80);
begin
  FOwnContext.Add(TALExprVar80Sym.Create(AName, @AVar));
end;

{*********************************************************************************}
procedure TALEasyEvaluator.AddFunc(const AName: AnsiString; AFunc: TALFloat32Func);
begin
  FOwnContext.Add(TALExprFloat32FuncSym.Create(AName, AFunc));
end;

{*********************************************************************************}
procedure TALEasyEvaluator.AddFunc(const AName: AnsiString; AFunc: TALFloat64Func);
begin
  FOwnContext.Add(TALExprFloat64FuncSym.Create(AName, AFunc));
end;

{*********************************************************************************}
procedure TALEasyEvaluator.AddFunc(const AName: AnsiString; AFunc: TALFloat80Func);
begin
  FOwnContext.Add(TALExprFloat80FuncSym.Create(AName, AFunc));
end;

{*********************************************************************************}
procedure TALEasyEvaluator.AddFunc(const AName: AnsiString; AFunc: TALUnary32Func);
begin
  FOwnContext.Add(TALExprUnary32FuncSym.Create(AName, AFunc));
end;

{*********************************************************************************}
procedure TALEasyEvaluator.AddFunc(const AName: AnsiString; AFunc: TALUnary64Func);
begin
  FOwnContext.Add(TALExprUnary64FuncSym.Create(AName, AFunc));
end;

{*********************************************************************************}
procedure TALEasyEvaluator.AddFunc(const AName: AnsiString; AFunc: TALUnary80Func);
begin
  FOwnContext.Add(TALExprUnary80FuncSym.Create(AName, AFunc));
end;

{**********************************************************************************}
procedure TALEasyEvaluator.AddFunc(const AName: AnsiString; AFunc: TALBinary32Func);
begin
  FOwnContext.Add(TALExprBinary32FuncSym.Create(AName, AFunc));
end;

{**********************************************************************************}
procedure TALEasyEvaluator.AddFunc(const AName: AnsiString; AFunc: TALBinary64Func);
begin
  FOwnContext.Add(TALExprBinary64FuncSym.Create(AName, AFunc));
end;

{**********************************************************************************}
procedure TALEasyEvaluator.AddFunc(const AName: AnsiString; AFunc: TALBinary80Func);
begin
  FOwnContext.Add(TALExprBinary80FuncSym.Create(AName, AFunc));
end;

{***********************************************************************************}
procedure TALEasyEvaluator.AddFunc(const AName: AnsiString; AFunc: TALTernary32Func);
begin
  FOwnContext.Add(TALExprTernary32FuncSym.Create(AName, AFunc));
end;

{***********************************************************************************}
procedure TALEasyEvaluator.AddFunc(const AName: AnsiString; AFunc: TALTernary64Func);
begin
  FOwnContext.Add(TALExprTernary64FuncSym.Create(AName, AFunc));
end;

{***********************************************************************************}
procedure TALEasyEvaluator.AddFunc(const AName: AnsiString; AFunc: TALTernary80Func);
begin
  FOwnContext.Add(TALExprTernary80FuncSym.Create(AName, AFunc));
end;

{*******************************}
procedure TALEasyEvaluator.Clear;
begin
  FOwnContext.FHashMap.Clear;
end;

{*********************************************************}
procedure TALEasyEvaluator.Remove(const AName: AnsiString);
begin
  FOwnContext.Remove(AName);
end;

end.


