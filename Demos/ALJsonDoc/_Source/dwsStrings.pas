{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsStrings;

{$I dws.inc}

interface

const
  // Constants of "System.pas"
  SYS_INTEGER = 'Integer';
  SYS_FLOAT = 'Float';
  SYS_STRING = 'String';
  SYS_BOOLEAN = 'Boolean';
  SYS_VARIANT = 'Variant';
  SYS_VOID = 'void';
  SYS_RESULT = 'Result';
  SYS_SELF = 'Self';
  SYS_INTERNAL = 'Internal';
  SYS_SYSTEM = 'System';
  SYS_DEFAULT = 'Default';
  SYS_OBJECT = 'Object';
  SYS_ANY = 'Any';
  SYS_TCLASS = 'TClass';
  SYS_TOBJECT = 'TObject';
  SYS_TOBJECT_CREATE = 'Create';
  SYS_TOBJECT_DESTROY = 'Destroy';
  SYS_TOBJECT_FREE = 'Free';
  SYS_TOBJECT_CLASSNAME = 'ClassName';
  SYS_TOBJECT_CLASSTYPE = 'ClassType';
  SYS_TOBJECT_CLASSPARENT = 'ClassParent';
  SYS_EXCEPTION = 'Exception';
  SYS_EXCEPTION_MESSAGE = 'Message';
  SYS_EXCEPTION_MESSAGE_FIELD = 'FMessage';
  SYS_EXCEPTION_STACKTRACE = 'StackTrace';
  SYS_EXCEPTION_DEBUGGER_FIELD = 'FDebuggerField';
  SYS_EASSERTIONFAILED = 'EAssertionFailed';
  SYS_EDELPHI = 'EDelphi';
  SYS_EDELPHI_EXCEPTIONCLASS = 'ExceptionClass';
  SYS_EDELPHI_EXCEPTIONCLASS_FIELD = 'FExceptionClass';
  SYS_TCUSTOMATTRIBUTE = 'TCustomAttribute';
  SYS_IINTERFACE = 'IInterface';

  SYS_COMPILER_VERSION = 'CompilerVersion';

  SYS_MainModule = '*MainModule*';

  SYS_EXTERNAL_ARRAY = '[]';

  // Compiler switches
  SWI_INCLUDE_LONG = 'INCLUDE';
  SWI_INCLUDE_SHORT = 'I';
  SWI_INCLUDE_ONCE = 'INCLUDE_ONCE';
  SWI_FILTER_LONG = 'FILTER';
  SWI_FILTER_SHORT = 'F';
  SWI_RESOURCE_LONG = 'RESOURCE';
  SWI_RESOURCE_SHORT = 'R';
  SWI_DEFINE = 'DEFINE';
  SWI_UNDEF = 'UNDEF';
  SWI_IFDEF = 'IFDEF';
  SWI_IFNDEF = 'IFNDEF';
  SWI_IF = 'IF';
  SWI_ENDIF = 'ENDIF';
  SWI_ELSE = 'ELSE';
  SWI_HINT = 'HINT';
  SWI_HINTS = 'HINTS';
  SWI_WARNING = 'WARNING';
  SWI_WARNINGS = 'WARNINGS';
  SWI_ERROR = 'ERROR';
  SWI_FATAL = 'FATAL';
  SWI_REGION = 'REGION';
  SWI_ENDREGION = 'ENDREGION';
  SWI_CODEGEN = 'CODEGEN';

  // COMPILER ERRORS
  //
  // These are currently implemented as constants for various considerations.
  // If you wish to localize those error messages, you may want to make them
  // into resourcestrings, though be aware that at this point you'll likely be
  // on your own as far as interpreting translated error messages goes.
  // So sticking to english-only compiler error message is encouraged ^_^

const

  // Missing Tokens
  CPE_XxxExpected = ' expected.';
  CPE_Unexpected_X = 'Unexpected "%s".';
  CPE_X_ExpectedBut_Y_Found = '%s expected but %s found.';
  CPE_CommaExpected = '"," expected.';
  CPE_SemiExpected = '";" expected.';
  CPE_BrackLeftExpected = '"(" expected.';
  CPE_BrackRightExpected = '")" expected.';
  CPE_ArrayBracketRightExpected = '"]" expected';
  CPE_ArrayBracketLeftExpected = '"[" expected';
  CPE_AtExpected = '"@" expected';
  CPE_UnexpectedAt = 'unexpected "@"';
  CPE_CurlyRightExpected = '"}" expected';
  CPE_ColonExpected = 'Colon ":" expected';
  CPE_DotExpected = 'Dot "." expected';
  CPE_NameExpected = 'Name expected';
  CPE_ProcOrFuncExpected = 'PROCEDURE or FUNCTION expected';
  CPE_EqualityExpected = '"=" expected';
  CPE_InExpected = 'IN expected';
  CPE_SetExpected = 'Set expected';
  CPE_ArrayExpected = 'Array expected';
  CPE_StringExpected = 'String expected';
  CPE_BeginExpected = 'BEGIN expected';
  CPE_VariableExpected = 'Variable expected';
  CPE_ForExpected = 'FOR expected';
  CPE_ToOrDowntoExpected = 'TO or DOWNTO expected';
  CPE_ToExpected = 'TO expected';
  CPE_DoExpected = 'DO expected';
  CPE_ThenExpected = 'THEN expected';
  CPE_ElseExpected = 'ELSE expected';
  CPE_OfExpected = 'OF expected';
  CPE_ExpressionExpected = 'Expression expected';
  CPE_DotDotExpected = '".." expected';
  CPE_EndExpected = 'END expected';
  CPE_AssignExpected = '":=" expected';
  CPE_ArrayBracketOrClassExpected = '"[" or class expected';
  CPE_OnOffExpected = 'ON/OFF expected';
  CPE_UnexpectedSection = 'Unexpected section declaration "%s"';
  CPE_UnexpectedStatement = 'Unexpected statement';
  CPE_UnexpectedImplementationInInterface = 'Unexpected implementation in interface section';
  CPE_UnexpectedEnd = 'Unexpected END';
  CPE_ClassExpected = 'CLASS expected';
  CPE_OnlyOneFieldExpectedForExternal = 'Only one field expected for EXTERNAL name';
  CPE_ExternalArrayForStaticMethodsOnly = 'External array is supported only for static methods';
  CPE_ExternalVariablesMustBeGlobal = 'External variables must be global';
  CPE_ExternalClassVariablesInitializationIsNotSupported = 'External class variables initialization is not supported';
  CPE_ExternalPropertyNoArguments = 'External properties cannot have arguments';

  // ReadName
  CPE_UnknownName = 'Unknown name "%s"';
  CPE_UnknownNameDotName = 'Unknown name "%s.%s"';
  CPE_UnknownType = 'Unknown type "%s"';
  CPE_UnknownUnit = 'Unknown unit "%s"';
  CPE_MemberSymbolNotVisible = 'Member symbol "%s" is not visible from this scope';

  // Class declaration errors
  CPE_CantOverrideNotInherited = 'No method "%s" found in class: "override" not applicable';
  CPE_CantOverrideNotVirtual = 'Inherited method "%s" isn''t virtual. "override" not applicable';
  CPE_CantOverrideWrongParameterList = 'Parameter list doesn''t match the inherited method';
  CPE_CantOverrideWrongResultType = 'Result type doesn''t match the inherited method';
  CPE_CantOverrideWrongMethodType = '"override" not applicable between class and instance methods';
  CPE_CantOverrideWrongFuncKind = 'Cannot override a %s with a %s';
  CPE_CantFinalWithoutOverride = 'Cannot mark "final" without overriding';
  CPE_CantOverrideFinal = 'Method "%s" marked as final. "override" not applicable';
  CPE_CantReintroduce = 'Method "%s" isn''t overlapping a virtual method';
  CPE_ReintroduceWarning = 'Method "%s" overlaps a virtual method';
  CPE_FieldRedefined = 'There is already a field with name "%s"';
  CPE_PropertyRedefined = 'There is already a property with name "%s"';
  CPE_MethodRedefined = 'There is already a method with name "%s"';
  CPE_ClassOperatorRedefined = 'Class operator already defined for type "%s"';
  CPE_ClassVarRedefined = 'There is already a class variable with name "%s"';
  CPE_ClassConstRedefined = 'There is already a class const with name "%s"';
  CPE_ImplClassNameExpected = 'Class name expected';
  CPE_ImplInvalidClass = '"%s" is not a method of class "%s"';
  CPE_ImplAbstract = '"%s.%s" is declared "abstract", no implementation allowed';
  CPE_StructureIsNotExternal = '"%s" is not external';
  CPE_NonVirtualAbstract = '"abstract" is only valid for virtual methods';
  CPE_NonConstructorDefault = 'Only a constructor can be marked as default';
  CPE_DefaultConstructorAlreadyDefined = 'Class "%s" already has "%s" as default constructor';
  CPE_AbstractClassUsage = 'Trying to use an abstract class';
  CPE_AbstractMethodUsage = 'Trying to call an abstract method';
  CPE_ImplClassExpected = 'Declaration should start with "class"';
  CPE_ImplNotClassExpected = 'Declaration shouldn''t start with "class"';
  CPE_InheritedOnlyAllowedInMethods = '"inherited" only allowed in methods';
  CPE_InheritedWithoutName = 'Name expected after "inherited"';
  CPE_InheritedMethodNotFound = 'Method "%s" not found in ancestor class';
  CPE_StaticMethodExpected = 'Class method or constructor expected';
  CPE_UnexpectedConstructor = 'Constructor invoked on instance outside of constructor';
  CPE_UnexpectedDestructor = 'Destructor can only be invoked on instance';
  CPE_UnexpectedMethodImplementation = 'Unexpected method implementation';
  CPE_WriteOnlyProperty = 'Cannot read a write only property';
  CPE_ReadOnlyProperty = 'Cannot set a value for a read-only property';
  CPE_CantReadProperty = 'Property cannot be read-accessed';
  CPE_ObjectReferenceExpected = 'Object reference needed to read/write an object field';
  CPE_StaticPropertyWriteExpected = 'Write access of property should be a static method';
  CPE_StaticPropertyReadExpected = 'Read access of property should be a static method';
  CPE_OnlyNonVirtualClassMethodsAsStatic = 'Only non-virtual class methods can be marked as static';
  CPE_UnknownClass = 'Class "%s" not found';
  CPE_NotAClass = '"%s" is not a class';
  CPE_NotAProperty = '"%s" is not a property';
  CPE_CannotDemotePropertyVisibility = 'Cannot demote property visibility';
  CPE_ClassForwardAlreadyExists = 'There is already a forward declaration of the "%s" class';
  CPE_ClassAlreadyDefined = 'Class "%s" already defined';
  CPE_ClassWasNotPartial = 'Previous declaration of class was not "partial"';
  CPE_ClassPartialModifiersNotMatched = 'Modifiers do not match previous "partial" declaration of class';
  CPE_FuncForwardAlreadyExists = 'There is already a forward declaration of this function';
  CPE_ForwardNotImplemented = 'The function "%s" was forward declared but not implemented';
  CPE_CantImplementAFunctionType = 'Cannot implement a function type';
  CPE_ClassIsSealed = 'Class "%s" is sealed, inheriting is not allowed';
  CPE_ClassIsStaticNoInstantiation = 'Class "%s" is static, instantiation not allowed';
  CPE_ClassIsStaticNoInstances = 'Class "%s" is static, no instances allowed';
  CPE_ClassAncestorNotStatic = 'Class "%s" is not static, cannot inherit as static';
  CPE_ClassAncestorDoesNotMatch = 'Class ancestor does not match with previous declaration';
  CPE_ClassExternalAncestorMustBeExternalOrObject = 'External classes must inherit from an external class or Object';
  CPE_MustBeSubClassOf = 'Subclass of %s expected';
  CPE_MethodOrPropertyExpected = 'Method or property declaration expected';
  CPE_OverloadableOperatorExpected = 'Overloadable operator expected';
  CPE_OverloadAlreadyExists = 'An overload already exists for this operator and types';
  CPE_SingleParameterExpected = 'Single parameter expected';
  CPE_UsesExpected = '"USES" expected';
  CPE_FieldMethodUnknown = 'Field/method "%s" not found';
  CPE_IncompatibleType = 'Field/method "%s" has an incompatible type';
  CPE_IncompatibleParameters = 'Method "%s" has incompatible parameters';
  CPE_ProcedureMethodExpected = 'Procedure expected';
  CPE_FunctionMethodExpected = 'Function expected';
  CPE_FunctionOrValueExpected = 'Function or value expected';
  CPE_InvalidNumberOfArguments = 'Method "%s" has a wrong number of arguments';
  CPE_InvalidParameterType = 'Method "%s" has an incompatible parameter type';
  CPE_ReadOrWriteExpected = 'Neither "read" nor "write" directive found';
  CPE_IncompatibleWriteSymbol = 'Symbol "%s" has an incompatible type';
  CPE_ClassNotCompletelyDefined = 'Class "%s" isn''t defined completely';
  CPE_MethodNotImplemented = 'Method "%s" of class "%s" not implemented';
  CPE_CantWriteProperty = 'Cannot write properties of complex type (record, array)';
  CPE_CantUseCombinedAssignmentOnProperty = 'Cannot use combined assignment on property';
  CPE_MultipleDefaultProperties = '"%s" already has "%s" as default property';
  CPE_NoDefaultPropertyAllowed = 'No default property is allowed here';
  CPE_ParamsExpected = 'Parameters expected';
  CPE_NoParamsExpected = 'No parameters expected';
  CPE_NoProtectedVisibilityForHelpers = 'Helpers do not supported "protected" visibility specifier';
  CPE_HelpersNotAllowedForDelegates = 'Helpers not allowed for delegates or function pointers';
  CPE_HelperExpected = '"helper" expected';
  CPE_AnonymousMethodsNotAllowedHere = 'Anonymous methods not allowed here';
  CPE_AnonymousClassesMustBeFullyDefined = 'Anonymous classes must be fully defined';
  CPE_AnonymousClassNotAllowed = 'Anonymous class not allowed by compiler options';

  CPE_AttributeConstructorExpected = 'Attribute constructor expected';
  CPE_DanglingAttribute = 'Dangling attribute declaration';

  // Record declaration
  CPE_AnonymousRecordMethodsMustBeInline = 'Anonymous record methods must be inline';
  CPE_NoProtectedVisibilityForRecords = 'Records do not supported "protected" visibility specifier';
  CPE_RecordFieldsMustBeBeforeMethods = 'Record fields must be declared before record methods';

  // Interface declaration
  CPE_InterfaceAlreadyDefined = 'Interface "%s" already defined';
  CPE_NotAnInterface = '"%s" is not an interface';
  CPE_MissingMethodForInterface = 'Missing matching method "%s" for interface "%s"';
  CPE_InterfaceAlreadyImplemented = 'Interface "%s" already implemented';
  CPE_InterfaceNotCompletelyDefined = 'Interface "%s" isn''t defined completely';
  CPE_InterfaceForwardAlreadyExists = 'There is already a forward declaration of the "%s" interface';

  // CompareFuncSymbols
  CPE_FunctionExpected = 'Declaration should be "function"';
  CPE_ProcedureExpected = 'Declaration should be "procedure"';
  CPE_MethodExpected = 'Declaration should be "method"';
  CPE_ConstructorExpected = 'Declaration should be "constructor"';
  CPE_DestructorExpected = 'Declaration should be "destructor"';
  CPE_BadResultType = 'Result type should be "%s"';
  CPE_BadNumberOfParameters = 'Expected %d parameters (instead of %d)';
  CPE_BadParameterName = 'Parameter %d - Name "%s" expected';
  CPE_BadParameterType = 'Parameter %d - Type "%s" expected (instead of "%s")';
  CPE_IncompatibleParameterTypes = 'Incompatible parameter types - "%s" expected (instead of "%s")';
  CPE_VarParameterExpected = 'Parameter %d (%s) - Var-parameter expected';
  CPE_ConstParameterExpected = 'Parameter %d (%s) - Const-parameter expected';
  CPE_ValueParameterExpected = 'Parameter %d (%s) - Value-parameter expected';
  CPE_MismatchingParameterDefaultValues = 'Parameter %d (%s) - default value at implementation does not match declaration or forward';
  CPE_VarParameterForbidden = 'Parameter %d - Var-parameter forbidden';

  // Arrays
  CPE_ArrayBoundNotAConstant = 'Bound isn''t a constant expression';
  CPE_ArrayBoundNotOrdinal = 'Bound isn''t of an ordinal type';
  CPE_LowerBoundGreaterThanUpperBound = 'Lower bound is greater than upper bound';
  CPE_ArrayBoundsOfDifferentTypes = 'Array bounds are of different types';
  CPE_ArrayIndexMismatch = 'Array index expected "%s" but got "%s"';
  CPE_ArrayMethodRestrictedToDynamicArrays = 'Array method "%s" is restricted to dynamic arrays';
  CPE_ArrayDoesNotHaveNaturalSortOrder = 'Array does not have a natural sort order';

  // Sets
  CPE_SetTooLargeForCastToInteger = 'Set has too many elements for cast to integer';

  // Assign
  CPE_RightSideNeedsReturnType = 'Assignment''s right-side-argument has no return type';
  CPE_CantWriteToLeftSide = 'Cannot assign a value to the left-side argument';

  // Function/Procedures
  CPE_FunctionTypeExpected = 'Function type expected';
  CPE_IncompatibleParameterType = 'Type of parameter %d don''t match the declaration';
  CPE_InvalidResultType = 'Invalid type "%s" for function result';
  CPE_NoResultTypeExpected = 'No result type expected';
  CPE_DuplicateExternal = 'An external function named "%s" is already declared';

  CPE_NameAlreadyExists = 'Name "%s" already exists';
  CPE_NameIsReserved = 'Name "%s" is reserved';
  CPE_TypeExpected = 'Type expected';
  CPE_InvalidType = '%s is not a Type';
  CPE_UnknownMember = 'There is no accessible member with name "%s"';
  CPE_NoMemberExpected = 'No member expected';
  CPE_NoArrayExpected = 'Not an array';
  CPE_NoMethodExpected = 'Not a method';
  CPE_NoIndicesExpectedForOpenArray = 'No indices expected for open array';
  CPE_InvalidInstruction = 'Invalid Instruction - function or assignment expected';
  CPE_ConstantInstruction = 'Constant Instruction - has no effect';
  CPE_EndOfBlockExpected = 'End of block expected';
  CPE_ContructorExpected = 'Constructor expected';
  CPE_TooManyArguments = 'Too many arguments';
  CPE_TooFewArguments = 'More arguments expected';
  CPE_NoArgumentsExpected = 'No arguments expected';
  CPE_WrongArgumentType = 'Argument %d expects type "%s"';
  CPE_WrongArgumentType_Long = 'Argument %d expects type "%s" instead of "%s"';
  CPE_NoDefaultProperty = 'Class "%s" has no default property';
  CPE_ConstVarParam = 'Argument %d (%s) cannot be passed as Var-parameter';
  CPE_OnlyVariablesAsVarParam = 'Only a variable can be be passed as Var-parameter';
  CPE_MustExplicitOverloads = 'Overloaded procedure "%s" must be marked with the "overload" directive';
  CPH_ShouldExplicitOverload = 'Overloaded method "%s" should be marked with the "overload" directive';
  CPE_NoMatchingOverloadDeclaration = 'There is no overloaded version of "%s" declared with these arguments';
  CPE_NoMatchingOverloadForCall = 'There is no overloaded version of "%s" that can be called with these arguments';
  CPE_MatchingOverload = 'Overload of "%s" will be ambiguous with a previously declared version';
  CPE_OverloadNotAllowed = 'Overload not allowed';
  CPE_ClassMethodExpected = 'Class method expected';
  CPE_ClassMemberExpected = 'Class member expected';
  CPE_RecordTypeExpected = 'Record type expected';
  CPE_RecordTypeNotFullyDefined = 'Record type "%s" is not fully defined';

  CPE_InvalidOperands = 'Invalid Operands';
  CPE_IncompatibleOperands = 'Incompatible operands';

  CPE_FunctionOptimizationFailed = 'Evaluation of "%s" failed: %s %s';

  CPH_VariableDeclaredButNotUsed = 'Variable "%s" declared but not used';
  CPH_VariableDeclaredButNotWrittenTo = 'Variable "%s" declared but never written to';
  CPH_PrivateFieldDeclaredButNotUsed = 'Private field "%s" declared but never used';
  CPH_PrivateMethodDeclaredButNotUsed = 'Private method "%s" declared but never used';
  CPH_PrivateVirtualMethodCantBeOverridden = 'Private virtual methods cannot be overridden';
  CPH_ResultNotUsed = 'Result is never used';
  CPH_RedundantVisibilitySpecifier = 'Redundant specifier, visibility is already "%s"';
  CPH_RedundantFunctionCall = 'Redundant function call';
  CPH_NameAmbiguousInScopeContext = 'Name "%s" could be ambiguous in its scope context';
  CPH_ReferenceTypeParamAsVarButNeverWrittenTo = '"%s" parameter is a reference type passed as VAR, but never written to';
  CPH_ReferenceTypeParamAsConst = '"%s" parameter is a reference type passed as CONST';
  CPH_UnitAlreadyReferred = 'Unit "%s" redeclared';
  CPH_UnitAlreadyReferredInInterface = 'Unit "%s" already declared in interface section';

  CPH_OfObjectIsLegacy = 'OF OBJECT modifier is legacy and ignored';
  CPE_OfObjectExpected = 'OF OBJECT expected';
  CPH_ReferenceToIsLegacy = 'REFERENCE TO modifier is legacy and ignored';

  CPH_CaseDoesNotMatchDeclaration = '"%s" does not match case of declaration ("%s")';

  CPH_CallConventionIsNotSupportedAndIgnored = 'Call convention "%s" is not supported and ignored';

  CPH_UnnamedEnumerationElement = 'Enumeration element is unnamed or out of range';

  CPH_EmptyThenBlock = 'Empty THEN block';
  CPH_EmptyElseBlock = 'Empty ELSE block';

  // TypeCheck
  CPE_BooleanExpected = 'Boolean expected';
  CPE_IntegerExpected = 'Integer expected';
  CPE_FloatExpected = 'Float expected';
  CPE_VariantExpected = 'Simple type expected';
  CPE_EnumerationExpected = 'Enumeration expected';
  CPE_NumericalExpected = 'Numerical operand expected';
  CPE_BooleanOrIntegerExpected = 'Boolean or integer operand expected';
  CPE_ObjectExpected = 'Object expected';
  CPE_ClassRefExpected = 'Class reference expected';
  CPE_InterfaceExpected = 'Interface expected';
  CPE_ExceptionObjectExpected = 'Exception object expected';

  CPE_IntegerCastInvalid = 'Cannot cast this type to "Integer"';
  CPE_IncompatibleTypes = 'Incompatible types: "%s" and "%s"';
  CPE_AssignIncompatibleTypes = 'Incompatible types: Cannot assign "%s" to "%s"';
  CPE_RangeIncompatibleTypes = 'Range start and range stop are of incompatible types: "%s" and "%s"';
  CPE_TypeCouldNotBeInferenced = 'Type could not be inferenced';
  CPE_RangeTooLarge = 'Range is too large';

  CPE_LocalFunctionAsDelegate = 'Local procedure/function cannot be used as delegate';

  // Connector
  CPE_ConnectorCall = 'Method "%s" not found in connector "%s"';
  CPE_NilConnectorCall = 'Attempt to call on a nil connector';
  CPE_NilConnectorRead = 'Attempt to read on a nil connector';
  CPE_NilConnectorWrite = 'Attempt to write on a nil connector';
  CPE_MethodConnectorParams = 'Method "%s" of connector "%s" does not accepts those parameters';
  CPE_ConnectorParams = 'Connector "%s" does not accepts those parameters';
  CPE_ConnectorMember = 'Member "%s" readonly or not found in connector "%s"';
  CPE_ConnectorTypeMismatch = 'Type mismatch in connector';
  CPE_ConnectorIndex = 'No index access in connector "%s"';
  CPE_ConnectorTooManyArguments = 'Too many arguments for connector call (%d)';
  CPE_ConnectorCantBeSpecialized = 'Connector "%s" cannot be specialized';
  CPE_ConnectorInvalidSpecifier = 'Connector "%s" specialization to "%s" failed';

  // Others
  CPE_CompilationAborted = 'Compilation aborted';

  CPE_ConstantExpressionExpected = 'Constant expression expected';
  CPE_IntegerExpressionExpected = 'Integer expression expected';
  CPE_OrdinalExpressionExpected = 'Ordinal expression expected';
  CPE_InvalidConstType = 'Invalid const type "%s"';
  CPE_ConstantCannotBeWrittenTo = 'Constant "%s" cannot be written to';

  CPE_CompilerSwitchUnknown = 'Compiler switch "%s" unknown';

  CPE_FlagEnumerationCantHaveUserValues = 'Flags enumerations cannot have user values';
  CPE_EnumerationElementOverflow = 'Enumeration element overflow';

  CPE_IncludeFileNotFound = 'Could not find file "%s" on input paths';
  CPE_IncludeFileExpected = 'Name of include file expected';
  CPE_IncludeItemUnknown = 'Include item "%s" unknown';
  CPE_IncludeItemExpected = 'Include item expected';

  CPE_BreakOutsideOfLoop = '"Break" outside of loop';
  CPE_ContinueOutsideOfLoop = '"Continue" outside of loop';
  CPE_ExitInFinally = '"Exit" not allowed in "Finally" block';
  CPE_BreakContinueInFinally = 'Direct "Break" or "Continue" not allowed in "Finally" block';

  CPE_UnbalancedConditionalDirective = 'Unbalanced conditional directive';
  CPE_UnfinishedConditionalDirective = 'Unfinished conditional directive';
  CPE_UnexpectedEndOfFileForUnfinishedComment = 'Unexpected end of file (unfinished comment)';
  CPE_UnexpectedEndOfFileForUnfinishedDirective = 'Unexpected end of file (unfinished directive)';

  CPE_UnexpectedEqGtrForLambdaStatement = 'Unexpected "=>" for a lambda statement';

  CPE_TypeIsUnknown = 'Type "%s" unknown';
  CPE_TypeForParamNotFound = 'Type "%s" not found for parameter "%s"';
  CPE_LazyParamCantBeVarOrConst = 'lazy parameter cannot be var or const';
  CPE_LazyParamCantHaveDefaultValue = 'lazy parameter cannot have a default value';
  CPE_LazyParamCantBeFunctionPointer = 'Lazy parameter cannot be a function pointer';
  CPE_VarParamCantHaveDefaultValue = 'var parameter cannot have a default value';
  CPE_ConstParamCantHaveDefaultValue = 'const parameter cannot have a default value';
  CPE_DefaultValueRequired = 'Default value required';
  CPE_OpenArrayParamMustBeConst = 'open array parameter must be const';
  CPE_OpenArrayParamElementsMustBeConst = 'open array parameter elements must be "const"';
  CPE_FieldExists = 'There is already a field with name "%s"';
  CPE_PropertyExists = 'There is already a property with name "%s"';
  CPE_MethodExists = 'There is already a method with name "%s"';
  CPE_AssignementToFORLoopVariable = 'Assignment to FOR-Loop variable';
  CPE_FORLoopMustBeLocalVariable = 'For loop control variable must be simple local variable';

  CPW_Deprecated = '"%s" has been deprecated';
  CPW_DeprecatedWithMessage = '"%s" has been deprecated: %s';
  CPW_InfiniteLoop = 'Infinite loop';
  CPW_ConstantCondition = 'Constant condition';
  CPW_UnReachableCode = 'Unreachable code';
  CPW_IncludeOnceWithDifferentCase = 'Filename case does not match: "%s" already included as "%s"';
  CPW_ForwardIsImplicit = '"forward" is implicit here';
  CPW_ForwardIsMeaningless = '"forward" is meaningless for external functions';
  CPW_PropertyWriterDoesNothing = 'Property writer does nothing';

  CPE_NoResultTypeRequired = 'No result type required';
  CPE_ResultTypeExpected = 'Result type expected';
  CPE_NoResultRequired = 'No result required';

  CPE_CanNotOverride = 'Method %s not found in parent class. Cannot override';
  CPE_InvalidArgCombination = 'Invalid argument combination';
  CPE_InvalidArgumentType = 'Invalid argument type';

  CPE_FieldAlreadySet = 'Field has already been set';

  CPE_DivisionByZero = 'Division by zero';

  // Contracts
  CPE_PreconditionsMustBeInRootMethod = 'Preconditions must be defined in the root method only.';

  // Units
  CPE_UnitExpected = '"unit" expected';
  CPE_UnitNotFound = 'Unit "%s" referenced in unit "%s" not found';
  CPE_UnitNameDoesntMatch = 'Unit name does not match file name';
  CPE_UnitCircularReference = 'Circular referencing units detected';
  CPE_FilterDependsOnUnit = 'The filter "%s" depends on unit "%s" that is not available.';
  CPE_ResultTypeDependsOnUnit = 'The result-type "%s" depends on unit "%s" that is not available.';
  CPE_NoStaticSymbols = 'Invalid use of static symbols';

  // Filter
  CPE_NoFilterAvailable = 'There is no filter assigned to TDelphiWebScriptII.Config.Filter';

  // TOKENIZER ERRORS
  TOK_InvalidChar = 'Invalid character';
  TOK_EqualityExpected = '"=" expected.';
  TOK_NumberExpected = 'Number expected';
  TOK_HexDigitExpected = 'Hexadecimal digit expected';
  TOK_BinDigitExpected = 'Binary digit expected';
  TOK_NumberPointExponentExpected = 'Number, point or exponent expected';
  TOK_NumberExponentExpected = 'Number or exponent expected';
  TOK_NumberSignExpected = 'Number or minus expected';
  TOK_GreaterEqualityExpected = '">" or "=" expected';
  TOK_StringTerminationError = 'End of string constant not found (end of line)';
  TOK_HereDocTerminationError = 'End of string constant not found (end of file)';
  TOK_InvalidHexConstant = 'Invalid hexadezimal constant "%s"';
  TOK_InvalidCharConstant = 'Invalid char constant "%s"';
  TOK_InvalidIntegerConstant = 'Invalid integer constant "%s"';
  TOK_InvalidFloatConstant = 'Invalid floating point constant "%s"';
  TOK_GreaterThanExpected = '> expected';
  TOK_NameOfSwitchExpected = 'Name of compiler switch expected';

  // Constants of TMsgs in dwsErrors.pas
  MSG_DatatypeMissing = 'Invalid type: %s';
  MSG_MainModule = SYS_MainModule;
  MSG_MainFunction = '*Main*';
  MSG_DeprecatedEmptyMsg = '!';
  MSG_Info = 'Info: %s';
  MSG_Error = 'Error: %s';
  MSG_ScriptPosLine = 'line: %d';
  MSG_ScriptPosColumn = 'column: %d';
  MSG_ScriptPosFile = 'file: %s';
  MSG_Hint = 'Hint: %s';
  MSG_Warning = 'Warning: %s';
  MSG_CompileError = 'Compile Error: %s';
  MSG_SyntaxError = 'Syntax Error: %s';
  MSG_RuntimeError = 'Runtime Error: %s';

  // Runtime Errors, Exceptions
  // ==========================
  RTE_CantRunScript = 'Script compiled with errors. Cannot execute';
  RTE_ScriptAlreadyRunning = 'Script is already running';
  RTE_ScriptStillRunning = 'Script is still running';
  RTE_ScriptStopped = 'Script was stopped.';
  RTE_ScriptHasLiveExecutions = 'Script has %d live executions';
  RTE_StateInitializedExpected = 'ProgramState "psInitialized" expected.';
  RTE_StateReadyToRunExpected = 'ProgramState "psReadyToRun" expected.';

  RTE_InstanceOfAbstractClass = 'Trying to create an instance of an abstract class';
  RTE_ArrayUpperBoundExceeded = 'Upper bound exceeded! Index %d';
  RTE_ArrayLowerBoundExceeded = 'Lower bound exceeded! Index %d';
  RTE_ArrayLengthIncorrect = 'Array length is incorrect (%d)';
  RTE_ArrayLengthIncorrectForDimension = 'Array length is incorrect (%d) for dimension %d';
  RTE_ArrayInstanceExpected = 'Array instance expected';
  RTE_PositiveCountExpected = 'Positive count expected (got %d)';
  RTE_ForLoopStepShouldBeStrictlyPositive = 'FOR loop STEP should be strictly positive: %d';
  RTE_InvalidBreak = 'break without for/while/repeat or case';
  RTE_InvalidContinue = 'continue without for/while/repeat or case';
  RTE_ClassInstanceCastFailed = 'Cannot cast instance of type "%s" to class "%s"';
  RTE_MetaClassCastFailed = 'Cannot cast "%s" to class "%s"';
  RTE_OrdinalExpected = 'Ordinal expected';
  RTE_VariantCastFailed = 'Could not cast variant from "%s" to "%s" (%s)';

  RTE_ObjCastToIntfFailed = 'Class "%s" does not implement interface "%s"';
  RTE_IntfCastToObjFailed = 'Cannot cast interface of "%s" to class "%s"';
  RTE_IntfCastToIntfFailed = 'Cannot cast interface of "%s" to interface "%s"';
  RTE_IntfIsNil = 'Interface is nil';

  RTE_ObjectNotInstantiated = 'Object not instantiated';
  RTE_ObjectAlreadyDestroyed = 'Object already destroyed';
  RTE_ClassTypeIsNil = 'ClassType is nil';

  RTE_FuncPointerIsNil = 'Function pointer is nil';

  RTE_ScriptException = 'Script exception: %s';
  RTE_UserDefinedException = 'User defined exception';
  RTE_UserDefinedException_Msg = 'User defined exception: %s';
  RTE_AssertionFailed = 'Assertion failed%s%s';
  RTE_PreConditionFailed = 'Pre-condition failed in %s%s, %s';
  RTE_PostConditionFailed = 'Post-condition failed in %s%s, %s';

  RTE_UnHandledExternalCall = 'Unhandled call to external symbol "%s" from%s';

  RTE_UnauthorizedFilePath = 'Unauthorized file path: "%s"';

  // Connectors
  RTE_ConnectorCallFailed = 'Connector Call "%s" failed';
  RTE_ConnectorReadError = 'ConnectorRead error';
  RTE_ConnectorWriteError = 'ConnectorWrite error';

  // Stack
  RTE_MaximalDatasizeExceeded = 'Maximal data size exceeded (%d Variants)';
  RTE_MaximalRecursionExceeded = 'Maximal recursion exceeded (%d calls)';
  RTE_MaximalExceptionDepthExceeded = 'Maximal exception depth exceeded (%d nested exceptions)';

  // TProgramInfo/TInfo
  RTE_VariableNotFound = 'Variable "%s" not found';
  RTE_FunctionNotFound = 'Function/Method "%s" not found';
  RTE_DatatypeNotFound = 'DataType "%s" not found';
  RTE_ClassMatchNotFound = 'Unable to register external object of type "%s". Class not found';

  RTE_OnlyVarSymbols = '.Vars[] cannot handle this symbol: "%s". Use .Func[] or .Method[] instead';
  RTE_OnlyFuncSymbols = '.Func[] cannot handle this symbol: "%s". Use .Vars[] instead';

  RTE_InvalidOp = 'Operation "IInfo.%s" not possible on a symbol of type "%s"';

  RTE_NoMemberOfClass = '"%s" is not a member of class "%s"';
  RTE_NoClassNoMethod = '"%s" is not a class and has no method "%s"';
  RTE_MethodNotFoundInClass = 'Method "%s" not found in class "%s"';
  RTE_FieldNotFoundInClass = 'Field "%s" not found in class "%s"';
  RTE_NoMemberOfArray = '"%s" is not a member of array "%s"';
  RTE_UnsupportedMemberOfClass = '"%s" is not supported as class member';

  RTE_CanNotReadComplexType = 'To read a value of complex type "%s" use .Data';
  RTE_CanNotSetValueForType = 'To write values of type "%s" use .Data';
  RTE_CanOnlyWriteBlocks = 'Use the .Data property of the type "%s" instead of "%s"';

  RTE_InvalidInputDataSize = 'Input data of invalid size: %d instead of %d';
  RTE_InvalidNumberOfParams = 'Invalid number of parameters (%d instead of %d) to call function %s';
  RTE_UseParameter = 'Use ''Parameter'' property to set parameter "%s" of Function "%s"';
  RTE_NoParameterFound = 'No parameter "%s" found in function "%s"';
  RTE_IncorrectParameterIndex = 'Incorrect parameter index %d';
  RTE_NoIndexFound = 'No index parameter "%s" found for property "%s"';

  RTE_NoRecordMemberFound = 'No member "%s" found in record "%s"';
  RTE_NoArray = '"%s" is not an array';
  RTE_TooManyIndices = 'Too many indices';
  RTE_TooFewIndices = 'Too few indices';
  RTE_NoRecordFields = 'Record has no field members';

  // Compiler Strings

  UNT_CircularReference = 'Circular reference to symbol "%s"';
  UNT_NameIsEmpty = 'Name property must not be empty';
  UNT_NameAlreadyExists = 'Name "%s" already exists';
  UNT_UnitNameNotDefined = 'Property %s.UnitName is undefined';
  UNT_ParameterNameAlreadyExists = 'Parameter name "%s" already exists';
  UNT_DependencyError = 'Dependency check from unit "%s" to unit "%s" failed: %s';
  UNT_PreviousNotOverloaded = 'Previous symbol "%s" not marked as overloaded';

  UNT_UnitGenerationError = 'TdwsUnit: "%s" -- %s';
  UNT_SymbolGenerationError = '%s: "%s" -- %s';

  UNT_DatatypeUnknown = 'DataType "%s" not found';
  UNT_DatatypeNotSpecified = 'DataType not specified for property "%s" of class "%s"';
  UNT_AutoInstantiateWithoutClass = 'AutoInstantiate is true but DataType "%s" is not a class';

  UNT_InterfaceAlreadyDefined = 'Interface already defined';
  UNT_InterfaceNameAlreadyDefined = 'The interface "%s" is already used for another symbol %s';

  UNT_ClassAlreadyDefined = 'Class already defined';
  UNT_ClassNameAlreadyDefined = 'The class "%s" is already used for another symbol %s';
  UNT_SuperClassUnknwon = 'Superclass "%s" not found';
  UNT_ReadAccessNotFound = 'ReadAccess "%s" not found';
  UNT_WriteAccessNotFound = 'WriteAccess "%s" not found';
  UNT_UsesAccessNotFound = 'UsesAccess "%s" not found';
  UNT_IncorrectOperatorOverload = 'Incorrect operator overload (%s)';

  UNT_InvalidArrayBounds = 'LowBound is higher than HighBound';

  UNT_CantChangeUnitname = 'Cannot change UnitName while property "Script" is assigned';

  UNT_InstancesNotSupportedInStaticUnits = 'Instances are not supported in static units';

  ADP_ChainIsFormingLoop = 'Adapter chain is forming a loop';
  ADP_IncompatibleAdapters = 'Incompatible Adapters: %s -> %s';

  // Debugger strings

  DBG_NotDebugging = '(not debugging)';
  DBG_NoResult = '(no result)';

  // AutoFix Actions

  AFA_NoLongerApplicable = 'Source code changed, no longer applicable';
  AFA_AddImplementation = 'Add implementation';

implementation

end.
