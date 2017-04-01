unit Grijjy.SymbolTranslator;

interface

{ Tries to translate a C++ symbol name (such as a function signature) to
  Pascal.

    Parameters:
      ASymbol: the C++ symbol to translate.

  Returns:
    The symbol translated to Pascal.

  The returned string is only an approximate translation. This function cannot
  translate all C++ constructs, so parts of the returned string may be
  untranslated. However, the result is good enough to be able to look it up
  in you Pascal source code. }
function goCppSymbolToPascal(const ASymbol: String): String;

implementation

uses
  System.SysUtils,
  System.Generics.Collections;

type
  TgoSymbolConverter = class // static
  private class var
    FTypeMap: TDictionary<String, String>;
    FOperatorMap: TDictionary<String, String>;
  private
    class function ParseFunctionHeading(var AStr: PChar): String; static;
    class function ParseQualifiedName(var AStr: PChar): String; overload; static; inline;
    class function ParseQualifiedName(var AStr: PChar; out AInSystem: Boolean): String; overload; static;
    class function ParseName(var AStr: PChar): String; static;
    class function ParseParams(var AStr: PChar): String; static;
    class function ParseParam(var AStr: PChar): String; static;
    class function ParseGenericArgs(var AStr: PChar): String; static;
    class function ParseGenericArg(var AStr: PChar): String; static;
    class function ParseRemainder(var AStr: PChar): String; static;
    class procedure SkipWhitespace(var AStr: PChar); static; inline;
    class function ConvertOperatorName(const AName: String): String; static;
    class function ConvertStaticArray(const AStr: String): String; static;
    class function ConvertSet(const AStr: String): String; static;
    class function ConvertCtorDtor(const AStr: String): String; static;
    class function ConvertSpecialFunctionName(const AStr: String): String; static;
  public
    class constructor Create;
    class destructor Destroy;
  public
    class function CppToPascal(const ASymbol: String): String; static;
  end;

function goCppSymbolToPascal(const ASymbol: String): String;
begin
  Result := TgoSymbolConverter.CppToPascal(ASymbol);
end;

{ TgoSymbolConverter }

class function TgoSymbolConverter.ConvertCtorDtor(const AStr: String): String;
{ Convert constructor and destructor.
  * Foo.TBar.TBar -> Foo.TBar.Create
  * Foo.TBar.~TBar -> Foo.TBar.Destroy }
var
  I: Integer;
  TypeName: String;
begin
  I := AStr.LastIndexOf('.');
  if (I < 0) then
    Exit(AStr);

  if (I < (AStr.Length - 1)) and (AStr.Chars[I + 1] = '~') then
    Result := AStr.Substring(0, I) + '.Destroy'
  else
  begin
    TypeName := AStr.Substring(I);
    Result := AStr.Substring(0, I);
    if (Result.EndsWith(TypeName)) then
      Result := Result + '.Create'
    else
      Result := AStr;
  end;
end;

class function TgoSymbolConverter.ConvertOperatorName(
  const AName: String): String;
begin
  if (not FOperatorMap.TryGetValue(AName, Result)) then
    Result := AName;
end;

class function TgoSymbolConverter.ConvertSet(const AStr: String): String;
{ Set<XYZ, ...> -> set of XYZ }
var
  I: Integer;
begin
  I := AStr.IndexOf(',');
  if (I < 0) then
    Result := AStr
  else
    Result := 'set of ' + AStr.Substring(4, I - 4);
end;

class function TgoSymbolConverter.ConvertSpecialFunctionName(
  const AStr: String): String;
{ Convert constructor and destructor.
  * Foo.Finalization() -> Foo.finalization }
begin
  if (AStr.EndsWith('initialization()')) then
    Result := AStr.Substring(0, AStr.Length - 16) + 'initialization'
  else if (AStr.EndsWith('Finalization()')) then
    Result := AStr.Substring(0, AStr.Length - 14) + 'finalization'
  else
    Result := AStr;
end;

class function TgoSymbolConverter.ConvertStaticArray(
  const AStr: String): String;
{ StaticArray<XYZ, N> -> array [0..N-1] of XYZ }
var
  CommaPos, N: Integer;
begin
  CommaPos := AStr.IndexOf(',');
  if (CommaPos < 0) then
    Exit(AStr);

  N := StrToIntDef(AStr.Substring(CommaPos + 1, AStr.Length - CommaPos - 2), 0);
  if (N = 0) then
    Exit(AStr);

  Result := 'array [0..' + IntToStr(N - 1) + '] of ' + AStr.Substring(12, CommaPos - 12);
end;

class function TgoSymbolConverter.CppToPascal(const ASymbol: String): String;
{ Symbol -> FunctionHeading ('::' FunctionHeading)*
  This allows for nested routines. For example:
    Foo(int)::Bar(float)
  Translates to
    Foo(Integer).Bar(Single)
  Where Bar is a nested routine inside the Foo routine. }
var
  P: PChar;
begin
  P := PChar(ASymbol);
  if (P^ = #0) then
    Exit('');

  Result := ParseFunctionHeading(P);
  while (P[0] = ':') and (P[1] = ':') do
  begin
    Inc(P, 2);
    Result := Result + '.' + ParseFunctionHeading(P);
  end;
end;

class constructor TgoSymbolConverter.Create;
begin
  FTypeMap := TDictionary<String, String>.Create;

  { C++ types }
  FTypeMap.Add('bool', 'Boolean');
  FTypeMap.Add('signed char', 'ShortInt');
  FTypeMap.Add('unsigned char', 'Byte');
  FTypeMap.Add('char', 'Byte');
  FTypeMap.Add('wchar_t', 'Char');
  FTypeMap.Add('char16_t', 'Char');
  FTypeMap.Add('char32_t', 'UCS4Char');
  FTypeMap.Add('short', 'SmallInt');
  FTypeMap.Add('short int', 'SmallInt');
  FTypeMap.Add('signed short', 'SmallInt');
  FTypeMap.Add('signed short int', 'SmallInt');
  FTypeMap.Add('unsigned short', 'Word');
  FTypeMap.Add('unsigned short int', 'Word');
  FTypeMap.Add('int', 'Integer');
  FTypeMap.Add('signed', 'Integer');
  FTypeMap.Add('signed int', 'Integer');
  FTypeMap.Add('unsigned', 'Cardinal');
  FTypeMap.Add('unsigned int', 'Cardinal');
  FTypeMap.Add('long', 'LongInt');
  FTypeMap.Add('long int', 'LongInt');
  FTypeMap.Add('signed long', 'LongInt');
  FTypeMap.Add('signed long int', 'LongInt');
  FTypeMap.Add('unsigned long', 'LongWord');
  FTypeMap.Add('unsigned long int', 'LongWord');
  FTypeMap.Add('long long', 'Int64');
  FTypeMap.Add('long long int', 'Int64');
  FTypeMap.Add('signed long long', 'Int64');
  FTypeMap.Add('signed long long int', 'Int64');
  FTypeMap.Add('unsigned long long', 'UInt64');
  FTypeMap.Add('unsigned long long int', 'UInt64');
  FTypeMap.Add('float', 'Single');
  FTypeMap.Add('double', 'Double');
  FTypeMap.Add('long double', 'Extended');

  { Delphi types }
  FTypeMap.Add('UnicodeString', 'String');

  FOperatorMap := TDictionary<String, String>.Create;
  FOperatorMap.Add('_op_Implicit', 'operator_Implicit');
  FOperatorMap.Add('_op_Explicit', 'operator_Explicit');
  FOperatorMap.Add('_op_UnaryNegation', 'operator_Negative');
  FOperatorMap.Add('_op_UnaryPlus', 'operator_Positive');
  FOperatorMap.Add('_op_Increment', 'operator_Inc');
  FOperatorMap.Add('_op_Decrement', 'operator_Dec');
  FOperatorMap.Add('_op_LogicalNot', 'operator_LogicalNot');
  FOperatorMap.Add('_op_Trunc', 'operator_Trunc');
  FOperatorMap.Add('_op_Round', 'operator_Round');
  FOperatorMap.Add('_op_In', 'operator_In');
  FOperatorMap.Add('_op_Equality', 'operator_Equal');
  FOperatorMap.Add('_op_Inequality', 'operator_NotEqual');
  FOperatorMap.Add('_op_GreaterThan', 'operator_GreaterThan');
  FOperatorMap.Add('_op_GreaterThanOrEqual', 'operator_GreaterThanOrEqual');
  FOperatorMap.Add('_op_LessThan', 'operator_LessThan');
  FOperatorMap.Add('_op_LessThanOrEqual', 'operator_LessThanOrEqual');
  FOperatorMap.Add('_op_Addition', 'operator_Add');
  FOperatorMap.Add('_op_Subtraction', 'operator_Subtract');
  FOperatorMap.Add('_op_Multiply', 'operator_Multiply');
  FOperatorMap.Add('_op_Division', 'operator_Divide');
  FOperatorMap.Add('_op_IntDivide', 'operator_IntDivide');
  FOperatorMap.Add('_op_Modulus', 'operator_Modulus');
  FOperatorMap.Add('_op_LeftShift', 'operator_LeftShift');
  FOperatorMap.Add('_op_RightShift', 'operator_RightShift');
  FOperatorMap.Add('_op_LogicalAnd', 'operator_LogicalAnd');
  FOperatorMap.Add('_op_LogicalOr', 'operator_LogicalOr');
  FOperatorMap.Add('_op_ExclusiveOr', 'operator_LogicalXor');
  FOperatorMap.Add('_op_BitwiseAnd', 'operator_BitwiseAnd');
  FOperatorMap.Add('_op_BitwiseOr', 'operator_BitwiseOr');
  FOperatorMap.Add('_op_BitwiseXOR', 'operator_BitwiseXor');
end;

class destructor TgoSymbolConverter.Destroy;
begin
  FTypeMap.Free;
  FOperatorMap.Free;
end;

class function TgoSymbolConverter.ParseFunctionHeading(var AStr: PChar): String;
{ FunctionHeading -> QualifiedName '(' Params ')' }
begin
  Result := ParseQualifiedName(AStr);
  Result := ConvertCtorDtor(Result);

  if (AStr^ <> '(') then
    Exit(Result + ParseRemainder(AStr));

  Inc(AStr);
  Result := Result + '(' + ParseParams(AStr);

  if (AStr^ <> ')') then
    Result := Result + ParseRemainder(AStr)
  else
  begin
    Inc(AStr);
    Result := Result + ')';
  end;

  Result := ConvertSpecialFunctionName(Result);
end;

class function TgoSymbolConverter.ParseGenericArg(var AStr: PChar): String;
{ GenericArg -> ['(' QualifiedName ')'] Param
  The QualifiedName can be used in typecasts inside generic arguments, as in:
    (Foo::Bar)0 }
begin
  if (AStr^ = '(') then
  begin
    Inc(AStr);
    Result := ParseQualifiedName(AStr); { Ignore the typecast }
    if (AStr^ = ')') then
      Inc(AStr)
    else
      Exit(Result + ParseRemainder(AStr));
  end;
  Result := ParseParam(AStr);
end;

class function TgoSymbolConverter.ParseGenericArgs(var AStr: PChar): String;
{ GenericArgs -> GenericArg (',' GenericArg)* }
begin
  Result := ParseGenericArg(AStr);
  while (AStr^ = ',') do
  begin
    Inc(AStr);
    SkipWhitespace(AStr);
    Result := Result + ', ' + ParseGenericArg(AStr);
  end;
end;

class function TgoSymbolConverter.ParseName(var AStr: PChar): String;
{ Name -> NameChar+ ['<' GenericArgs '>']
  NameChar -> <any character except some delimiters> }
var
  P: PChar;
  I, J: Integer;
begin
  P := AStr;
  while True do
  begin
    case P^ of
      #0..#32,
      '<', '>',
      '(', ')',
      ',', ':':
        Break;
    else
      Inc(P);
    end;
  end;

  SetString(Result, AStr, P - AStr);

  { Rename class operators }
  if (AStr[0] = '_') and (AStr[1] = 'o') and (AStr[2] = 'p') and (AStr[3] = '_') then
    Result := ConvertOperatorName(Result);

  AStr := P;
  if (AStr^ = '<') then
  begin
    { When using generic arguments, the name may end with a discriminator,
      like "__1". Strip this. }
    I := Result.IndexOf('__');
    if (I > 0) then
    begin
      J := I + 2;
      while (J < Result.Length) and (Result.Chars[J] >= '0') and (Result.Chars[J] <= '9') do
        Inc(J);
      if (J = Result.Length) then
        Result := Result.Substring(0, I);
    end;

    Inc(AStr);
    Result := Result + '<' + ParseGenericArgs(AStr);
    SkipWhitespace(AStr);
    if (AStr^ <> '>') then
      Result := Result + ParseRemainder(AStr)
    else
    begin
      Inc(AStr);
      Result := Result + '>';
    end;
  end;
end;

class function TgoSymbolConverter.ParseParam(var AStr: PChar): String;
{ Param -> QualifiedName+
  A parameter can take multiple names in these cases:
  * The C++ type takes multiple names, like 'signed char' or 'unsigned long long'.
  * The parameter has qualifiers, like 'const&' }
var
  CppName, Name, Suffix: String;
  InSystem, IsReference: Boolean;
  I: Integer;
begin
  CppName := ParseQualifiedName(AStr, InSystem);

  while (AStr^ <> #0) and (AStr^ <= ' ') do
  begin
    SkipWhitespace(AStr);
    Name := ParseQualifiedName(AStr);
    if (Name <> '') and (Name <> 'const&') then
      CppName := CppName + ' ' + Name;
  end;

  { Check for references (int&) and pointers (int*, int** etc).
    Make exception for 'void*' which translates to Pointer. }
  IsReference := CppName.EndsWith('&');
  if IsReference then
    CppName := CppName.Substring(0, CppName.Length - 1);

  I := CppName.IndexOf('*');
  if (I < 0) then
    Suffix := ''
  else if (CppName.StartsWith('void*')) then
  begin
    Suffix := CppName.Substring(5);
    CppName := 'Pointer';
  end
  else
  begin
    Suffix := CppName.Substring(I);
    CppName := CppName.Substring(0, I);
  end;

  { Convert (C++) type to Delphi type }
  if (not FTypeMap.TryGetValue(CppName, Result)) then
    Result := CppName;

  { Special cases:
    * For names in the System namespace:
      * DynamicArray<XYZ> -> array of XYZ
      * StaticArray<XYZ, N> -> array [0..N-1] of XYZ
      * DelphiInterface<XYZ> -> XYZ
      * Set<XYZ, ...> -> set of XYZ
    * XYZ& -> var XYZ }

  if InSystem then
  begin
    if (Result.StartsWith('DynamicArray<')) then
      Result := 'array of ' + Result.Substring(13, Result.Length - 14)
    else if (Result.StartsWith('StaticArray<')) then
      Result := ConvertStaticArray(Result)
    else if (Result.StartsWith('DelphiInterface<')) then
      Result := Result.Substring(16, Result.Length - 17)
    else if (Result.StartsWith('Set<')) then
      Result := ConvertSet(Result);
  end;

  if (IsReference) then
    Result := 'var ' + Result;

  Result := Result + Suffix;
end;

class function TgoSymbolConverter.ParseParams(var AStr: PChar): String;
{ Params -> Param (',' Param)*
  Convert special case "TVarRec*, Integer" to "array of const" }
var
  PrevIsVarRec: Boolean;
  Param: String;
begin
  Result := ParseParam(AStr);
  PrevIsVarRec := (Result = 'TVarRec*');
  while (AStr^ = ',') do
  begin
    Inc(AStr);
    SkipWhitespace(AStr);
    Param := ParseParam(AStr);
    if (PrevIsVarRec) then
    begin
      if (Param = 'Integer') then
      begin
        Result := Result.Substring(0, Result.Length - 8);
        Result := Result + 'array of const';
      end
      else
        Result := Result + ', ' + Param;
      PrevIsVarRec := False;
    end
    else
    begin
      Result := Result + ', ' + Param;
      PrevIsVarRec := (Param = 'TVarRec*');
    end;
  end;
end;

class function TgoSymbolConverter.ParseQualifiedName(var AStr: PChar): String;
var
  Dummy: Boolean;
begin
  Result := ParseQualifiedName(AStr, Dummy);
end;

class function TgoSymbolConverter.ParseQualifiedName(var AStr: PChar;
  out AInSystem: Boolean): String;
{ QualifiedName -> Name ('::' Name)*
  For brevity, we ignore the System namespace in names that start with it. }
begin
  Result := ParseName(AStr);
  AInSystem := (Result = 'System') and (AStr[0] = ':') and (AStr[1] = ':');
  if AInSystem then
  begin
    Inc(AStr, 2);
    Result := ParseName(AStr);
  end;

  while (AStr[0] = ':') and (AStr[1] = ':') do
  begin
    Inc(AStr, 2);
    Result := Result + '.' + ParseName(AStr);
  end;
end;

class function TgoSymbolConverter.ParseRemainder(var AStr: PChar): String;
{ Is called when string cannot be parsed.
  Just returns the remainder of the string as-is. }
var
  P: PChar;
begin
  P := AStr;
  while (P^ <> #0) do
    Inc(P);
  SetString(Result, AStr, P - AStr);
  AStr := P;
end;

class procedure TgoSymbolConverter.SkipWhitespace(var AStr: PChar);
begin
  while (AStr^ <> #0) and (AStr^ <= ' ') do
    Inc(AStr);
end;

end.
