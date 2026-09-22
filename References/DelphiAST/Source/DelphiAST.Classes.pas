unit DelphiAST.Classes;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  SysUtils, Generics.Collections, SimpleParser.Lexer.Types, DelphiAST.Consts;

type
  EParserException = class(Exception)
  strict private
    FFileName: string;
    FLine, FCol: Integer;
  public
    constructor Create(Line, Col: Integer; const FileName, Msg: string); reintroduce;

    property FileName: string read FFileName;
    property Line: Integer read FLine;
    property Col: Integer read FCol;
  end;
  
  TAttributeEntry = TPair<TAttributeName, string>;
  PAttributeEntry = ^TAttributeEntry;

  TSyntaxNodeClass = class of TSyntaxNode;
  TSyntaxNode = class
  private
    FLineSeq: Integer;
    FCol: Integer;
    FLine: Integer;
    FFileName: string;
    function GetHasChildren: Boolean;
    function GetHasAttributes: Boolean;
    function TryGetAttributeEntry(const Key: TAttributeName; var AttributeEntry: PAttributeEntry): boolean;
  protected
    FAttributes: TArray<TAttributeEntry>;
    FChildNodes: TArray<TSyntaxNode>;
    FTyp: TSyntaxNodeType;
    FParentNode: TSyntaxNode;
  public
    constructor Create(Typ: TSyntaxNodeType);
    destructor Destroy; override;

    function Clone: TSyntaxNode; virtual;
    procedure AssignPositionFrom(const Node: TSyntaxNode);
    
    function GetAttribute(const Key: TAttributeName): string;
    function HasAttribute(const Key: TAttributeName): Boolean;
    procedure SetAttribute(const Key: TAttributeName; const Value: string);
    procedure ClearAttributes;

    function AddChild(Node: TSyntaxNode): TSyntaxNode; overload;
    function AddChild(Typ: TSyntaxNodeType): TSyntaxNode; overload;
    procedure DeleteChild(Node: TSyntaxNode);
    procedure ExtractChild(Node: TSyntaxNode);
    function FindNode(Typ: TSyntaxNodeType): TSyntaxNode; overload;
    // Searches for a node located along the path from the type of nodes
    // specified in the TypesPath parameter.
    // ntUnknown in the TypesPath parameter means a node of any type.
    // For example, for the branch presented below as XML
    // FindNode([ntAbsolute, ntValue, ntExpression, ntIdentifier]),
    // FindNode([ntAbsolute, ntUnknown, ntExpression, ntIdentifier]) è
    // FindNode([ntAbsolute, ntUnknown, ntUnknown, ntIdentifier])
    // return the IDENTIFIER node.
    // <VARIABLE line="9" col="3">
    //   <NAME line="9" col="3" value="ValueRec"/>
    //   <TYPE line="9" col="13" name="LongInt"/>
    //   <ABSOLUTE line="9" col="21">
    //     <VALUE line="9" col="30">
    //       <EXPRESSION line="9" col="30">
    //         <IDENTIFIER line="9" col="30" name="AValue"/>
    //       </EXPRESSION>
    //     </VALUE>
    //   </ABSOLUTE>
    // </VARIABLE>.
    function FindNode(const TypesPath: array of TSyntaxNodeType): TSyntaxNode; overload;
    property Attributes: TArray<TAttributeEntry> read FAttributes;
    property ChildNodes: TArray<TSyntaxNode> read FChildNodes;
    property HasAttributes: Boolean read GetHasAttributes;
    property HasChildren: Boolean read GetHasChildren;
    property Typ: TSyntaxNodeType read FTyp;
    property ParentNode: TSyntaxNode read FParentNode;

    property LineSeq: Integer read FLineSeq write FLineSeq;
    property Col: Integer read FCol write FCol;
    property Line: Integer read FLine write FLine;
    property FileName: string read FFileName write FFileName;
  end;

  TCompoundSyntaxNode = class(TSyntaxNode)
  private
    FEndCol: Integer;
    FEndLine: Integer;
  public
    function Clone: TSyntaxNode; override;

    property EndCol: Integer read FEndCol write FEndCol;
    property EndLine: Integer read FEndLine write FEndLine;
  end;

  TValuedSyntaxNode = class(TSyntaxNode)
  private
    FValue: string;
  public
    function Clone: TSyntaxNode; override;

    property Value: string read FValue write FValue;
  end;

  TCommentNode = class(TSyntaxNode)
  private
    FText: string;
  public
    function Clone: TSyntaxNode; override;

    property Text: string read FText write FText;
  end;

  TExpressionTools = class
  private
    class function CreateNodeWithParentsPosition(NodeType: TSyntaxNodeType; ParentNode: TSyntaxNode): TSyntaxNode;
  public
    class function ExprToReverseNotation(Expr: TList<TSyntaxNode>): TList<TSyntaxNode>; static;
    class procedure NodeListToTree(Expr: TList<TSyntaxNode>; Root: TSyntaxNode); static;
    class function PrepareExpr(ExprNodes: TList<TSyntaxNode>): TList<TSyntaxNode>; static;
    class procedure RawNodeListToTree(RawParentNode: TSyntaxNode; RawNodeList: TList<TSyntaxNode>; NewRoot: TSyntaxNode); static;
  end;

implementation

type
  TOperatorKind = (okUnary, okBinary);
  TOperatorAssocType = (atLeft, atRight);

  TOperatorInfo = record
    Typ: TSyntaxNodeType;
    Priority: Byte;
    Kind: TOperatorKind;
    AssocType: TOperatorAssocType;
  end;

  TOperators = class
  strict private
    class function GetItem(Typ: TSyntaxNodeType): TOperatorInfo; static;
  public
    class function IsOpName(Typ: TSyntaxNodeType): Boolean;
    class property Items[Typ: TSyntaxNodeType]: TOperatorInfo read GetItem; default;
  end;

const
  OperatorsInfo: array [0..29] of TOperatorInfo =
    ((Typ: ntAddr;         Priority: 1; Kind: okUnary;  AssocType: atRight),
     (Typ: ntDeref;        Priority: 1; Kind: okUnary;  AssocType: atLeft),
     (Typ: ntGeneric;      Priority: 1; Kind: okBinary; AssocType: atRight),
     (Typ: ntIndexed;      Priority: 1; Kind: okUnary;  AssocType: atLeft),
     (Typ: ntDot;          Priority: 2; Kind: okBinary; AssocType: atRight),
     (Typ: ntCall;         Priority: 3; Kind: okBinary; AssocType: atRight),
     (Typ: ntUnaryMinus;   Priority: 5; Kind: okUnary;  AssocType: atRight),
     (Typ: ntNot;          Priority: 6; Kind: okUnary;  AssocType: atRight),
     (Typ: ntMul;          Priority: 7; Kind: okBinary; AssocType: atRight),
     (Typ: ntFDiv;         Priority: 7; Kind: okBinary; AssocType: atRight),
     (Typ: ntDiv;          Priority: 7; Kind: okBinary; AssocType: atRight),
     (Typ: ntMod;          Priority: 7; Kind: okBinary; AssocType: atRight),
     (Typ: ntAnd;          Priority: 7; Kind: okBinary; AssocType: atRight),
     (Typ: ntShl;          Priority: 7; Kind: okBinary; AssocType: atRight),
     (Typ: ntShr;          Priority: 7; Kind: okBinary; AssocType: atRight),
     (Typ: ntAs;           Priority: 7; Kind: okBinary; AssocType: atRight),
     (Typ: ntAdd;          Priority: 8; Kind: okBinary; AssocType: atRight),
     (Typ: ntSub;          Priority: 8; Kind: okBinary; AssocType: atRight),
     (Typ: ntOr;           Priority: 8; Kind: okBinary; AssocType: atRight),
     (Typ: ntXor;          Priority: 8; Kind: okBinary; AssocType: atRight),
     (Typ: ntEqual;        Priority: 9; Kind: okBinary; AssocType: atRight),
     (Typ: ntNotEqual;     Priority: 9; Kind: okBinary; AssocType: atRight),
     (Typ: ntLower;        Priority: 9; Kind: okBinary; AssocType: atRight),
     (Typ: ntGreater;      Priority: 9; Kind: okBinary; AssocType: atRight),
     (Typ: ntLowerEqual;   Priority: 9; Kind: okBinary; AssocType: atRight),
     (Typ: ntGreaterEqual; Priority: 9; Kind: okBinary; AssocType: atRight),
     (Typ: ntIn;           Priority: 9; Kind: okBinary; AssocType: atRight),
     (Typ: ntNotIn;        Priority: 9; Kind: okBinary; AssocType: atRight),
     (Typ: ntIs;           Priority: 9; Kind: okBinary; AssocType: atRight),
     (Typ: ntIsNot;        Priority: 9; Kind: okBinary; AssocType: atRight));

{ TOperators }

class function TOperators.GetItem(Typ: TSyntaxNodeType): TOperatorInfo;
var
  i: Integer;
begin
  for i := 0 to High(OperatorsInfo) do
    if OperatorsInfo[i].Typ = Typ then
      Exit(OperatorsInfo[i]);
end;

class function TOperators.IsOpName(Typ: TSyntaxNodeType): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(OperatorsInfo) do
    if OperatorsInfo[i].Typ = Typ then
      Exit(True);
  Result := False;
end;

function IsRoundClose(Typ: TSyntaxNodeType): Boolean; inline;
begin
  Result := Typ = ntRoundClose;
end;

function IsRoundOpen(Typ: TSyntaxNodeType): Boolean; inline;
begin
  Result := Typ = ntRoundOpen;
end;

class function TExpressionTools.ExprToReverseNotation(Expr: TList<TSyntaxNode>): TList<TSyntaxNode>;
var
  Stack: TStack<TSyntaxNode>;
  Node: TSyntaxNode;
begin
  Result := TList<TSyntaxNode>.Create;
  try
    Stack := TStack<TSyntaxNode>.Create;
    try
      for Node in Expr do
        if TOperators.IsOpName(Node.Typ) then
        begin
          while (Stack.Count > 0) and TOperators.IsOpName(Stack.Peek.Typ) and
            (((TOperators.Items[Node.Typ].AssocType = atLeft) and
            (TOperators.Items[Node.Typ].Priority >= TOperators.Items[Stack.Peek.Typ].Priority))
            or
            ((TOperators.Items[Node.Typ].AssocType = atRight) and
            (TOperators.Items[Node.Typ].Priority > TOperators.Items[Stack.Peek.Typ].Priority)))
          do
            Result.Add(Stack.Pop);

          Stack.Push(Node);
        end
        else if IsRoundOpen(Node.Typ) then
          Stack.Push(Node)
        else if IsRoundClose(Node.Typ) then
        begin
          while not IsRoundOpen(Stack.Peek.Typ) do
            Result.Add(Stack.Pop);

          // RoundOpen and RoundClose nodes are not needed anymore
          Stack.Pop.Free;
          Node.Free;

          if (Stack.Count > 0) and TOperators.IsOpName(Stack.Peek.Typ) then
            Result.Add(Stack.Pop);
        end else
          Result.Add(Node);

      while Stack.Count > 0 do
        Result.Add(Stack.Pop);
    finally
      Stack.Free;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

class procedure TExpressionTools.NodeListToTree(Expr: TList<TSyntaxNode>; Root: TSyntaxNode);
var
  Stack: TStack<TSyntaxNode>;
  Node, SecondNode: TSyntaxNode;
begin
  Stack := TStack<TSyntaxNode>.Create;
  try
    for Node in Expr do
    begin
      if TOperators.IsOpName(Node.Typ) then
        case TOperators.Items[Node.Typ].Kind of
          okUnary: Node.AddChild(Stack.Pop);
          okBinary:
            begin
              SecondNode := Stack.Pop;
              Node.AddChild(Stack.Pop);
              Node.AddChild(SecondNode);
            end;
        end;
      Stack.Push(Node);
    end;

    Root.AddChild(Stack.Pop);

    Assert(Stack.Count = 0);
  finally
    Stack.Free;
  end;
end;

class function TExpressionTools.PrepareExpr(ExprNodes: TList<TSyntaxNode>): TList<TSyntaxNode>;
var
  Node, PrevNode: TSyntaxNode;
begin
  Result := TList<TSyntaxNode>.Create;
  try
    Result.Capacity := ExprNodes.Count * 2;

    PrevNode := nil;
    for Node in ExprNodes do
    begin
      if Node.Typ = ntCall then
        Continue;

      if Assigned(PrevNode) and IsRoundOpen(Node.Typ) then
      begin
        if not TOperators.IsOpName(PrevNode.Typ) and not IsRoundOpen(PrevNode.Typ) then
          Result.Add(CreateNodeWithParentsPosition(ntCall, Node.ParentNode));

        if TOperators.IsOpName(PrevNode.Typ)
          and (TOperators.Items[PrevNode.Typ].Kind = okUnary)
          and (TOperators.Items[PrevNode.Typ].AssocType = atLeft)
        then
          Result.Add(CreateNodeWithParentsPosition(ntCall, Node.ParentNode));
      end;

      if Assigned(PrevNode) and (Node.Typ = ntTypeArgs) then
      begin
        if not TOperators.IsOpName(PrevNode.Typ) and (PrevNode.Typ <> ntTypeArgs) then
          Result.Add(CreateNodeWithParentsPosition(ntGeneric, Node.ParentNode));

        if TOperators.IsOpName(PrevNode.Typ)
          and (TOperators.Items[PrevNode.Typ].Kind = okUnary)
          and (TOperators.Items[PrevNode.Typ].AssocType = atLeft)
        then
          Result.Add(CreateNodeWithParentsPosition(ntGeneric, Node.ParentNode));
      end;

      if Node.Typ <> ntAlignmentParam then
        Result.Add(Node.Clone);
      PrevNode := Node;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

class function TExpressionTools.CreateNodeWithParentsPosition(NodeType: TSyntaxNodeType; ParentNode: TSyntaxNode): TSyntaxNode;
begin
  Result := TSyntaxNode.Create(NodeType);
  Result.AssignPositionFrom(ParentNode);
end;

class procedure TExpressionTools.RawNodeListToTree(RawParentNode: TSyntaxNode; RawNodeList: TList<TSyntaxNode>;
  NewRoot: TSyntaxNode);
var
  PreparedNodeList, ReverseNodeList: TList<TSyntaxNode>;
begin
  try
    PreparedNodeList := PrepareExpr(RawNodeList);
    try
      ReverseNodeList := ExprToReverseNotation(PreparedNodeList);
      try
        NodeListToTree(ReverseNodeList, NewRoot);
      finally
        ReverseNodeList.Free;
      end;
    finally
      PreparedNodeList.Free;
    end;
  except
    on E: Exception do
      raise EParserException.Create(NewRoot.Line, NewRoot.Col, NewRoot.FileName, E.Message);
  end;
end;

{ TSyntaxNode }

procedure TSyntaxNode.SetAttribute(const Key: TAttributeName; const Value: string);
var
  AttributeEntry: PAttributeEntry;
  len: Integer;
begin
  if not TryGetAttributeEntry(Key, AttributeEntry) then
  begin
    len := Length(FAttributes);
    SetLength(FAttributes, len + 1);
    AttributeEntry := @FAttributes[len];
    AttributeEntry^.Key := Key;
  end;
  AttributeEntry^.Value := Value;
end;

function TSyntaxNode.TryGetAttributeEntry(const Key: TAttributeName; var AttributeEntry: PAttributeEntry): boolean;
var
  i: integer;
begin
  for i := 0 to High(FAttributes) do
    if FAttributes[i].Key = Key then
    begin
      AttributeEntry := @FAttributes[i];
      Exit(True);
    end;

  Result := False;
end;

function TSyntaxNode.AddChild(Node: TSyntaxNode): TSyntaxNode;
begin
  Assert(Assigned(Node));

  SetLength(FChildNodes, Length(FChildNodes) + 1);
  FChildNodes[Length(FChildNodes) - 1] := Node;

  Node.FParentNode := Self;

  Result := Node;
end;

function TSyntaxNode.AddChild(Typ: TSyntaxNodeType): TSyntaxNode;
begin
  Result := AddChild(TSyntaxNode.Create(Typ));
end;

function TSyntaxNode.Clone: TSyntaxNode;
var
  i: Integer;
begin
  Result := TSyntaxNodeClass(Self.ClassType).Create(FTyp);

  SetLength(Result.FChildNodes, Length(FChildNodes));
  for i := 0 to High(FChildNodes) do
  begin
    Result.FChildNodes[i] := FChildNodes[i].Clone;
    Result.FChildNodes[i].FParentNode := Result;
  end;

  Result.FAttributes := Copy(FAttributes);
  Result.AssignPositionFrom(Self);
end;

constructor TSyntaxNode.Create(Typ: TSyntaxNodeType);
begin
  inherited Create;
  FTyp := Typ;
end;

procedure TSyntaxNode.ExtractChild(Node: TSyntaxNode);
var
  i: integer;
begin
  for i := 0 to High(FChildNodes) do
    if FChildNodes[i] = Node then
    begin
      if i < High(FChildNodes) then
        Move(FChildNodes[i + 1], FChildNodes[i], SizeOf(TSyntaxNode) * (Length(FChildNodes) - i - 1));
      SetLength(FChildNodes, High(FChildNodes));
      Break;
    end;
end;

procedure TSyntaxNode.DeleteChild(Node: TSyntaxNode);
begin
  ExtractChild(Node);
  Node.Free;
end;

destructor TSyntaxNode.Destroy;
var
  i: integer;
begin
  for i := 0 to High(FChildNodes) do
    FreeAndNil(FChildNodes[i]);
  inherited;
end;

function TSyntaxNode.FindNode(Typ: TSyntaxNodeType): TSyntaxNode;
var
  i: Integer;
begin
  for i := 0 to High(FChildNodes) do
    if FChildNodes[i].Typ = Typ then
      Exit(FChildNodes[i]);
  Result := nil;
end;

function TSyntaxNode.FindNode(const TypesPath: array of TSyntaxNodeType): TSyntaxNode;

  function FindNodeRecursively(Node: TSyntaxNode;
    const TypesPath: array of TSyntaxNodeType; TypeIndex: Integer): TSyntaxNode;
  var
    ChildNode: TSyntaxNode;
  begin
    Result := nil;
    for ChildNode in Node.ChildNodes do
      if TypesPath[TypeIndex] in [ChildNode.Typ] + [ntUnknown] then
      begin
        if TypeIndex < High(TypesPath) then
          Result := FindNodeRecursively(ChildNode, TypesPath, TypeIndex + 1)
        else
          Result := ChildNode;
        if Assigned(Result) then
          Exit;
      end;
  end;

begin
  if TypesPath[High(TypesPath)] <> ntUnknown then
    Result := FindNodeRecursively(Self, TypesPath, Low(TypesPath))
  else
    Result := nil;
end;

function TSyntaxNode.GetAttribute(const Key: TAttributeName): string;
var
  AttributeEntry: PAttributeEntry;
begin
  if TryGetAttributeEntry(Key, AttributeEntry) then
    Result := AttributeEntry^.Value
  else
    Result := '';
end;

function TSyntaxNode.GetHasAttributes: Boolean;
begin
  Result := Length(FAttributes) > 0;
end;

function TSyntaxNode.GetHasChildren: Boolean;
begin
  Result := Length(FChildNodes) > 0;
end;

function TSyntaxNode.HasAttribute(const Key: TAttributeName): Boolean;
var
  AttributeEntry: PAttributeEntry;
begin
  Result := TryGetAttributeEntry(Key, AttributeEntry);
end;

procedure TSyntaxNode.ClearAttributes;
begin
  SetLength(FAttributes, 0);
end;

procedure TSyntaxNode.AssignPositionFrom(const Node: TSyntaxNode);
begin
  FLineSeq := Node.LineSeq;
  FCol := Node.Col;
  FLine := Node.Line;
  FFileName := Node.FileName;
end;

{ TCompoundSyntaxNode }

function TCompoundSyntaxNode.Clone: TSyntaxNode;
begin
  Result := inherited;

  TCompoundSyntaxNode(Result).EndLine := Self.EndLine;
  TCompoundSyntaxNode(Result).EndCol := Self.EndCol;
end;

{ TValuedSyntaxNode }

function TValuedSyntaxNode.Clone: TSyntaxNode;
begin
  Result := inherited;

  TValuedSyntaxNode(Result).Value := Self.Value;
end;

{ TCommentNode }

function TCommentNode.Clone: TSyntaxNode;
begin
  Result := inherited;

  TCommentNode(Result).Text := Self.Text;
end;

{ EParserException }

constructor EParserException.Create(Line, Col: Integer; const FileName, Msg: string);
begin
  inherited Create(Msg);
  FFileName := FileName;
  FLine := Line;
  FCol := Col;
end;

end.