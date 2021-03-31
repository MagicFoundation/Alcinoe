{*******************************************************************************
ALAVLBinaryTree (Self-Balancing Binary Trees)
Binary trees that are self-balancing in the AVL sense
(the depth of any left branch differs by no more than
one from the depth of the right branch).
*******************************************************************************}
unit ALAVLBinaryTree;

interface

uses
  system.classes;

type

  {class defintion----------------}
  TALBaseAVLBinaryTreeNode = class;
  TALBaseAVLBinaryTree = class;

  {iterate function--------------------------------------------------}
  TALAVLBinaryTreeIterateFunc = procedure(aTree: TALBaseAVLBinaryTree;
                                          aNode: TALBaseAVLBinaryTreeNode;
                                          aExtData: Pointer;
                                          Var aContinue: Boolean);

  {TALBaseAVLBinaryTreeNode---------------}
  TALBaseAVLBinaryTreeNode = class(Tobject)
  Private
  Protected
    ChildNodes: array[Boolean] of TALBaseAVLBinaryTreeNode;
    Bal: -1..1;
    Procedure SaveToStream(Astream: Tstream); Virtual;
    Procedure LoadFromStream(Astream: Tstream); Virtual;
  Public
    Constructor Create; virtual;
  end;

  {TALBaseAVLBinaryTree---------------}
  TALBaseAVLBinaryTree = class(TObject)
  private
    FHead: TALBaseAVLBinaryTreeNode;
    FNodeCount: Integer;
  protected
    procedure FreeNodeObj(aNode: TALBaseAVLBinaryTreeNode); virtual;
    Function  CompareNode(IdVal: pointer; ANode: TALBaseAVLBinaryTreeNode): Integer; overload; Virtual; Abstract; {compares IdVal and aNode.keydata and returns 0 if they are equal. If IdVal is greater than aNode.keydata, returns an integer greater than 0. If aNode.keydata is less than IdVal, returns an integer less than 0.}
    Function  CompareNode(aNode1, ANode2: TALBaseAVLBinaryTreeNode): Integer; overload; Virtual; Abstract; {compares aNode1 and aNode2 and returns 0 if they are equal. If aNode1 is greater than aNode2, returns an integer greater than 0. If aNode1 is less than aNode2, returns an integer less than 0.}
    function  CreateNode: TALBaseAVLBinaryTreeNode; virtual; abstract;
    procedure InternalIterate(Action: TALAVLBinaryTreeIterateFunc; Up: Boolean; ExtData: Pointer); virtual;
    function  InternalAddNode(aNode: TALBaseAVLBinaryTreeNode): Boolean; virtual;
    Function  InternalExtractNode(IdVal: Pointer): TALBaseAVLBinaryTreeNode; virtual;
    Function  InternalDeleteNode(IdVal: Pointer): Boolean; virtual;
    Procedure InternalClear; Virtual;
    Function  InternalGetHead: TALBaseAVLBinaryTreeNode; virtual;
    Procedure InternalSaveToStream(Astream: Tstream); Virtual;
    Procedure InternalLoadFromStream(Astream: Tstream); Virtual;
    function  InternalFindNode(idVal: pointer): TALBaseAVLBinaryTreeNode; virtual;
    function  InternalFirst: TALBaseAVLBinaryTreeNode; virtual; {Return the smallest-value node in the tree}
    function  InternalLast: TALBaseAVLBinaryTreeNode; virtual; {Return the largest-value node in the tree}
    function  InternalNext(aNode: TALBaseAVLBinaryTreeNode): TALBaseAVLBinaryTreeNode; virtual; {Return the next node whose value is larger than aNode}
    function  InternalPrev(aNode: TALBaseAVLBinaryTreeNode): TALBaseAVLBinaryTreeNode; virtual; {Return the largest node whose value is smaller than aNode}
    Function  InternalGetNodeCount: integer; virtual;
  public
    Constructor Create; virtual;
    Destructor  Destroy; Override;
    procedure   Iterate(Action: TALAVLBinaryTreeIterateFunc; Up: Boolean; ExtData: Pointer); virtual;
    function    AddNode(aNode: TALBaseAVLBinaryTreeNode): Boolean; virtual;
    Function    ExtractNode(IdVal: Pointer): TALBaseAVLBinaryTreeNode; virtual;
    Function    DeleteNode(IdVal: Pointer): Boolean; virtual;
    Procedure   Clear; Virtual;
    Function    Head: TALBaseAVLBinaryTreeNode; virtual;
    Procedure   SaveToStream(Astream: Tstream); Virtual;
    Procedure   LoadFromStream(Astream: Tstream); Virtual;
    Procedure   SaveToFile(const AFilename: AnsiString); Virtual;
    Procedure   LoadFromFile(const AFilename: AnsiString); Virtual;
    function    FindNode(idVal: pointer): TALBaseAVLBinaryTreeNode; virtual;
    function    First: TALBaseAVLBinaryTreeNode; virtual; {Return the smallest-value node in the tree}
    function    Last: TALBaseAVLBinaryTreeNode; virtual; {Return the largest-value node in the tree}
    function    Next(aNode: TALBaseAVLBinaryTreeNode): TALBaseAVLBinaryTreeNode; virtual; {Return the next node whose value is larger than aNode}
    function    Prev(aNode: TALBaseAVLBinaryTreeNode): TALBaseAVLBinaryTreeNode; virtual; {Return the largest node whose value is smaller than aNode}
    Function    NodeCount: integer; virtual;
  end;

  {TALIntegerKeyAVLBinaryTreeNode--------------------------------}
  TALIntegerKeyAVLBinaryTreeNode = class(TALBaseAVLBinaryTreeNode)
  Private
  Protected
    Procedure SaveToStream(Astream: Tstream); override;
    Procedure LoadFromStream(Astream: Tstream); override;
  Public
    ID: Integer;
    Constructor Create; Override;
  end;

  {TALIntegerKeyAVLBinaryTree-------------------------}
  TALIntegerKeyAVLBinaryTree = class(TALBaseAVLBinaryTree)
  private
  protected
    Function CompareNode(IdVal: pointer; ANode: TALBaseAVLBinaryTreeNode): Integer; override; {compares IdVal and aNode.keydata and returns 0 if they are equal. If IdVal is greater than aNode.keydata, returns an integer greater than 0. If aNode.keydata is less than IdVal, returns an integer less than 0.}
    Function CompareNode(aNode1, ANode2: TALBaseAVLBinaryTreeNode): Integer; override; {compares aNode1 and aNode2 and returns 0 if they are equal. If aNode1 is greater than aNode2, returns an integer greater than 0. If aNode1 is less than aNode2, returns an integer less than 0.}
    function CreateNode: TALBaseAVLBinaryTreeNode; override;
  public
    function AddNode(aNode: TALIntegerKeyAVLBinaryTreeNode): Boolean; reintroduce; virtual;
    Function ExtractNode(IdVal: Integer): TALIntegerKeyAVLBinaryTreeNode; reintroduce; virtual;
    function DeleteNode(idVal: Integer): boolean; reintroduce; virtual;
    Function Head: TALIntegerKeyAVLBinaryTreeNode; Reintroduce; virtual;
    function FindNode(idVal: Integer): TALIntegerKeyAVLBinaryTreeNode; Reintroduce; virtual;
    function First: TALIntegerKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the smallest-value node in the tree}
    function Last: TALIntegerKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest-value node in the tree}
    function Next(aNode: TALIntegerKeyAVLBinaryTreeNode): TALIntegerKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the next node whose value is larger than aNode}
    function Prev(aNode: TALIntegerKeyAVLBinaryTreeNode): TALIntegerKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest node whose value is smaller than aNode}
  end;

  {TALCardinalKeyAVLBinaryTreeNode--------------------------------}
  TALCardinalKeyAVLBinaryTreeNode = class(TALBaseAVLBinaryTreeNode)
  Private
  Protected
    Procedure SaveToStream(Astream: Tstream); override;
    Procedure LoadFromStream(Astream: Tstream); override;
  Public
    ID: Cardinal;
    Constructor Create; Override;
  end;

  {TALCardinalKeyAVLBinaryTree-------------------------}
  TALCardinalKeyAVLBinaryTree = class(TALBaseAVLBinaryTree)
  private
  protected
    Function CompareNode(IdVal: pointer; ANode: TALBaseAVLBinaryTreeNode): Integer; override; {compares IdVal and aNode.keydata and returns 0 if they are equal. If IdVal is greater than aNode.keydata, returns an integer greater than 0. If aNode.keydata is less than IdVal, returns an integer less than 0.}
    Function CompareNode(aNode1, ANode2: TALBaseAVLBinaryTreeNode): Integer; override; {compares aNode1 and aNode2 and returns 0 if they are equal. If aNode1 is greater than aNode2, returns an integer greater than 0. If aNode1 is less than aNode2, returns an integer less than 0.}
    function CreateNode: TALBaseAVLBinaryTreeNode; override;
  public
    function AddNode(aNode: TALCardinalKeyAVLBinaryTreeNode): Boolean; reintroduce; virtual;
    Function ExtractNode(IdVal: Cardinal): TALCardinalKeyAVLBinaryTreeNode; reintroduce; virtual;
    function DeleteNode(idVal: Cardinal): boolean; reintroduce; virtual;
    Function Head: TALCardinalKeyAVLBinaryTreeNode; Reintroduce; virtual;
    function FindNode(idVal: Cardinal): TALCardinalKeyAVLBinaryTreeNode; Reintroduce; virtual;
    function First: TALCardinalKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the smallest-value node in the tree}
    function Last: TALCardinalKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest-value node in the tree}
    function Next(aNode: TALCardinalKeyAVLBinaryTreeNode): TALCardinalKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the next node whose value is larger than aNode}
    function Prev(aNode: TALCardinalKeyAVLBinaryTreeNode): TALCardinalKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest node whose value is smaller than aNode}
  end;

  {TALInt64KeyAVLBinaryTreeNode--------------------------------}
  TALInt64KeyAVLBinaryTreeNode = class(TALBaseAVLBinaryTreeNode)
  Private
  Protected
    Procedure SaveToStream(Astream: Tstream); override;
    Procedure LoadFromStream(Astream: Tstream); override;
  Public
    ID: Int64;
    Constructor Create; Override;
  end;

  {TALInt64KeyAVLBinaryTree----------------------------}
  TALInt64KeyAVLBinaryTree = class(TALBaseAVLBinaryTree)
  private
  protected
    Function CompareNode(IdVal: pointer; ANode: TALBaseAVLBinaryTreeNode): Integer; override; {compares IdVal and aNode.keydata and returns 0 if they are equal. If IdVal is greater than aNode.keydata, returns an integer greater than 0. If aNode.keydata is less than IdVal, returns an integer less than 0.}
    Function CompareNode(aNode1, ANode2: TALBaseAVLBinaryTreeNode): Integer; override; {compares aNode1 and aNode2 and returns 0 if they are equal. If aNode1 is greater than aNode2, returns an integer greater than 0. If aNode1 is less than aNode2, returns an integer less than 0.}
    function CreateNode: TALBaseAVLBinaryTreeNode; override;
  public
    function AddNode(aNode: TALInt64KeyAVLBinaryTreeNode): Boolean; reintroduce; virtual;
    Function ExtractNode(IdVal: Int64): TALInt64KeyAVLBinaryTreeNode; reintroduce; virtual;
    function DeleteNode(idVal: Int64): boolean; reintroduce; virtual;
    Function Head: TALInt64KeyAVLBinaryTreeNode; Reintroduce; virtual;
    function FindNode(idVal: int64): TALInt64KeyAVLBinaryTreeNode; Reintroduce; virtual;
    function First: TALInt64KeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the smallest-value node in the tree}
    function Last: TALInt64KeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest-value node in the tree}
    function Next(aNode: TALInt64KeyAVLBinaryTreeNode): TALInt64KeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the next node whose value is larger than aNode}
    function Prev(aNode: TALInt64KeyAVLBinaryTreeNode): TALInt64KeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest node whose value is smaller than aNode}
  end;

  {TALStringKeyAVLBinaryTreeNode--------------------------------}
  TALStringKeyAVLBinaryTreeNode = class(TALBaseAVLBinaryTreeNode)
  Private
  Protected
    Procedure SaveToStream(Astream: Tstream); override;
    Procedure LoadFromStream(Astream: Tstream); override;
  Public
    ID: AnsiString;
    Constructor Create; Override;
  end;

  {TALStringKeyAVLBinaryTreeCompareKeyFunct----------------------------------------------------------}
  TALStringKeyAVLBinaryTreeCompareKeyFunct = function (const aKey1, aKey2: AnsiString): Integer of object;

  {TALStringKeyAVLBinaryTree----------------------------}
  TALStringKeyAVLBinaryTree = class(TALBaseAVLBinaryTree)
  private
    FCaseSensitive: Boolean;
    FcompareKeyFunct: TALStringKeyAVLBinaryTreeCompareKeyFunct;
    procedure SetcaseSensitive(const Value: Boolean);
  protected
    Function CompareKeyCaseSensitive(Const aKey1, aKey2: AnsiString): Integer; {compares akey1 and akey2 and returns 0 if they are equal. If akey1 is greater than akey2, returns an integer greater than 0. If akey1 is less than akey2, returns an integer less than 0.}
    Function CompareKeyCaseInSensitive(Const aKey1, aKey2: AnsiString): Integer; {compares akey1 and akey2 and returns 0 if they are equal. If akey1 is greater than akey2, returns an integer greater than 0. If akey1 is less than akey2, returns an integer less than 0.}
    Function CompareNode(IdVal: pointer; ANode: TALBaseAVLBinaryTreeNode): Integer; override; {compares IdVal and aNode.keydata and returns 0 if they are equal. If IdVal is greater than aNode.keydata, returns an integer greater than 0. If aNode.keydata is less than IdVal, returns an integer less than 0.}
    Function CompareNode(aNode1, ANode2: TALBaseAVLBinaryTreeNode): Integer; override; {compares aNode1 and aNode2 and returns 0 if they are equal. If aNode1 is greater than aNode2, returns an integer greater than 0. If aNode1 is less than aNode2, returns an integer less than 0.}
    function CreateNode: TALBaseAVLBinaryTreeNode; override;
  public
    Constructor Create; override;
    function    AddNode(aNode: TALStringKeyAVLBinaryTreeNode): Boolean; reintroduce; virtual;
    function    ExtractNode(const IdVal: AnsiString): TALStringKeyAVLBinaryTreeNode; reintroduce; virtual;
    function    DeleteNode(const idVal: AnsiString): boolean; reintroduce; virtual;
    Function    Head: TALStringKeyAVLBinaryTreeNode; Reintroduce; virtual;
    function    FindNode(const idVal: AnsiString): TALStringKeyAVLBinaryTreeNode; Reintroduce; virtual;
    function    First: TALStringKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the smallest-value node in the tree}
    function    Last: TALStringKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest-value node in the tree}
    function    Next(aNode: TALStringKeyAVLBinaryTreeNode): TALStringKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the next node whose value is larger than aNode}
    function    Prev(aNode: TALStringKeyAVLBinaryTreeNode): TALStringKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest node whose value is smaller than aNode}
    Property    CaseSensitive: Boolean read FCaseSensitive write SetcaseSensitive default True;
  end;

implementation

uses
  System.Contnrs,
  System.sysUtils,
  ALString;

{Following stack declarations are used to avoid recursion in all tree
 routines. Because the tree is AVL-balanced, a stack size of 40
 allows at least 2**32 elements in the tree without overflowing the
 stack.}

const
  cALAVLBinaryTree_StackSize = 40;
  cALAVLBinaryTree_LeftChild = False;
  cALAVLBinaryTree_RightChild = True;

type
  TALAVLBinaryTree_StackNode = record
    Node : TALBaseAVLBinaryTreeNode;
    Comparison : Integer;
  end;
  TALAVLBinaryTree_StackArray = array[1..cALAVLBinaryTree_StackSize] of TALAVLBinaryTree_StackNode;


{*************************************************}
function AlAVLBinaryTree_Sign(I: Integer): Integer;
begin
  if I < 0 then Result := -1
  else if I > 0 then Result := +1
  else Result := 0;
end;

{***********************************************************************}
procedure AlAVLBinaryTree_DelBalance(var aNode: TALBaseAVLBinaryTreeNode;
                                     var SubTreeDec: Boolean;
                                     CmpRes: Integer);
var N1, N2: TALBaseAVLBinaryTreeNode;
    B1, B2: Integer;
    LR: Boolean;
begin
  CmpRes := AlAVLBinaryTree_Sign(CmpRes);
  if aNode.Bal = CmpRes then aNode.Bal := 0
  else if aNode.Bal = 0 then begin
    aNode.Bal := -CmpRes;
    SubTreeDec := False;
  end
  else begin
    LR := (CmpRes < 0);
    N1 := aNode.ChildNodes[LR];
    B1 := N1.Bal;
    if (B1 = 0) or (B1 = -CmpRes) then begin
      {Single RR or LL rotation}
      aNode.ChildNodes[LR] := N1.ChildNodes[not LR];
      N1.ChildNodes[not LR] := aNode;
      if B1 = 0 then begin
        aNode.Bal := -CmpRes;
        N1.Bal := CmpRes;
        SubTreeDec := False;
      end
      else begin
        aNode.Bal := 0;
        N1.Bal := 0;
      end;
      aNode := N1;
    end
    else begin
      {Double RL or LR rotation}
      N2 := N1.ChildNodes[not LR];
      B2 := N2.Bal;
      N1.ChildNodes[not LR] := N2.ChildNodes[LR];
      N2.ChildNodes[LR] := N1;
      aNode.ChildNodes[LR] := N2.ChildNodes[not LR];
      N2.ChildNodes[not LR] := aNode;
      if B2 = -CmpRes then aNode.Bal := CmpRes
      else aNode.Bal := 0;
      if B2 = CmpRes then N1.Bal := -CmpRes
      else N1.Bal := 0;
      aNode := N2;
      N2.Bal := 0;
    end;
  end;
end;

{***********************************************************************}
procedure AlAVLBinaryTree_InsBalance(var aNode: TALBaseAVLBinaryTreeNode;
                                     var SubTreeInc: Boolean;
                                     CmpRes: Integer);
var N1: TALBaseAVLBinaryTreeNode;
    N2: TALBaseAVLBinaryTreeNode;
    LR: Boolean;
begin
  CmpRes := AlAVLBinaryTree_Sign(CmpRes);
  if aNode.Bal = -CmpRes then begin
    aNode.Bal := 0;
    SubTreeInc := False;
  end
  else if aNode.Bal = 0 then aNode.Bal := CmpRes
  else begin
    LR := (CmpRes > 0);
    N1 := aNode.ChildNodes[LR];
    if N1.Bal = CmpRes then begin
      aNode.ChildNodes[LR] := N1.ChildNodes[not LR];
      N1.ChildNodes[not LR] := aNode;
      aNode.Bal := 0;
      aNode := N1;
    end
    else begin
      N2 := N1.ChildNodes[not LR];
      N1.ChildNodes[not LR] := N2.ChildNodes[LR];
      N2.ChildNodes[LR] := N1;
      aNode.ChildNodes[LR] := N2.ChildNodes[not LR];
      N2.ChildNodes[not LR] := aNode;
      if N2.Bal = CmpRes then aNode.Bal := -CmpRes
      else aNode.Bal := 0;
      if N2.Bal = -CmpRes then N1.Bal := CmpRes
      else N1.Bal := 0;
      aNode := N2;
    end;
    aNode.Bal := 0;
    SubTreeInc := False;
  end;
end;

{***************************************************************************}
procedure AlAVLBinaryTree_IterateDestroyNodeFunc(aTree: TALBaseAVLBinaryTree;
                                                 aNode: TALBaseAVLBinaryTreeNode;
                                                 aExtData: Pointer;
                                                 Var aContinue: Boolean);

begin
  aTree.FreeNodeObj(aNode);
  acontinue := True;
end;

{******************************************}
constructor TALBaseAVLBinaryTreeNode.Create;
begin
 ChildNodes[cALAVLBinaryTree_LeftChild] := nil;
 ChildNodes[cALAVLBinaryTree_RightChild] := nil;
 Bal := 0;
end;

{******************************************************************}
procedure TALBaseAVLBinaryTreeNode.LoadFromStream(Astream: Tstream);
begin
 //virtual
end;

{****************************************************************}
procedure TALBaseAVLBinaryTreeNode.SaveToStream(Astream: Tstream);
begin
 //virtual
end;

{**************************************}
Constructor TALBaseAVLBinaryTree.Create;
begin
  FHead := Nil;
  FNodeCount := 0;
  randomize;
  Inherited;
end;

{**************************************}
Destructor TALBaseAVLBinaryTree.Destroy;
begin
  InternalClear;
  Inherited;
end;

{*********************************************************************************}
procedure TALBaseAVLBinaryTree.InternalIterate(Action: TALAVLBinaryTreeIterateFunc;
                                               Up: Boolean;
                                               ExtData: Pointer);
var N1: TALBaseAVLBinaryTreeNode;
    N2: TALBaseAVLBinaryTreeNode;
    StackPos: Integer;
    Stack: TALAVLBinaryTree_StackArray;
    Continue: Boolean;
begin
  Continue := True;
  StackPos := 0;
  N1 := Fhead;
  repeat
    while Assigned(N1) do begin
      Inc(StackPos);
      Stack[StackPos].Node := N1;
      N1 := N1.ChildNodes[not Up];
    end;
    if StackPos = 0 then Exit;

    N1 := Stack[StackPos].Node;
    Dec(StackPos);
    N2 := N1;
    N1 := N1.ChildNodes[Up];

    Action(Self, N2, ExtData, Continue);
    if not continue then Exit;
  until False;
end;


{**************************************************************************************}
function TALBaseAVLBinaryTree.InternalAddNode(aNode: TALBaseAVLBinaryTreeNode): Boolean;
var N1: TALBaseAVLBinaryTreeNode;
    CmpRes: Integer;
    StackPos: Integer;
    Stack: TALAVLBinaryTree_StackArray;
    SubTreeInc: Boolean;
begin
  {exit if node is nil}
  if not Assigned(aNode) then begin
    Result := False;
    Exit;
  end;

  {Handle first node}
  N1 := FHead;
  if not Assigned(N1) then begin
    Fhead := aNode;
    Inc(FNodeCount);
    result := True;
    Exit;
  end;

  {Find where new node should fit in tree}
  StackPos := 0;
  CmpRes := 0;
  while Assigned(N1) do begin

    {compare node}
    CmpRes := CompareNode(aNode, N1);

    {node already exist, so exit}
    if CmpRes = 0 then begin
      Result := False;
      Exit;
    end;

    {Build the stack}
    Inc(StackPos);
    with Stack[StackPos] do begin
      Node := N1;
      Comparison := CmpRes;
    end;

    {continue the loop}
    N1 := N1.ChildNodes[CmpRes > 0];

  end;

  {Insert new node}
  Stack[StackPos].Node.ChildNodes[CmpRes > 0] := aNode;
  Inc(FNodeCount);
  result := True;

  {Unwind the stack and rebalance}
  SubTreeInc := True;
  while (StackPos > 0) and SubTreeInc do begin
    if StackPos = 1 then ALAVLBinaryTree_InsBalance(Fhead, SubTreeInc, Stack[1].Comparison)
    else with Stack[StackPos-1] do
      AlAVLBinaryTree_InsBalance(Node.ChildNodes[Comparison > 0], SubTreeInc, Stack[StackPos].Comparison);
    dec(StackPos);
  end;
end;

{*******************************************}
procedure TALBaseAVLBinaryTree.InternalClear;
begin
  InternalIterate(AlAVLBinaryTree_IterateDestroyNodeFunc,
                  True,
                  nil);
  FHead := nil;
  FNodeCount := 0;
end;

{**********************************************************************}
function TALBaseAVLBinaryTree.InternalGetHead: TALBaseAVLBinaryTreeNode;
begin
  Result := Fhead;
end;

{**************************************************************************}
procedure TALBaseAVLBinaryTree.FreeNodeObj(aNode: TALBaseAVLBinaryTreeNode);
begin
  aNode.Free;
end;

{******************************************************************************************}
function TALBaseAVLBinaryTree.InternalExtractNode(IdVal: Pointer): TALBaseAVLBinaryTreeNode;
var N1: TALBaseAVLBinaryTreeNode;
    N2: TALBaseAVLBinaryTreeNode;
    TmpNode: TALBaseAVLBinaryTreeNode;
    CmpRes: Integer;
    Found: Boolean;
    SubTreeDec: Boolean;
    StackPos: Integer;
    StackParentPos: integer;
    Stack: TALAVLBinaryTree_StackArray;
begin
  {exit if head is nil}
  N1 := Fhead;
  if not Assigned(N1) then begin
    result := nil;
    Exit;
  end;

  {Find node to delete and stack the nodes to reach it}
  Found := False;
  StackPos := 0;
  while not Found do begin

    {compare node}
    CmpRes := CompareNode(IdVal, N1);
    Inc(StackPos);

    {Found node}
    if CmpRes = 0 then begin
      with Stack[StackPos] do begin
        Node := N1;
        Comparison := -1;
      end;
      Found := True;
    end

    {not found yet, continue the search}
    else begin
      with Stack[StackPos] do begin
        Node := N1;
        Comparison := CmpRes;
      end;
      N1 := N1.ChildNodes[CmpRes > 0];

      {Node not found, then exit}
      if not Assigned(N1) then begin
        Result := nil;
        Exit;
      end;
    end;

  end;

  {save the position of the parent of the node to delete in the stack}
  StackParentPos := StackPos - 1;

  {Delete the node found}
  N2 := N1;
  if (not Assigned(N2.ChildNodes[cALAVLBinaryTree_RightChild])) or (not Assigned(N2.ChildNodes[cALAVLBinaryTree_LeftChild])) then begin
    {Node has at most one branch}
    Dec(StackPos);
    N1 := N2.ChildNodes[Assigned(N2.ChildNodes[cALAVLBinaryTree_RightChild])];
    if StackPos = 0 then Fhead := N1
    else with Stack[StackPos] do
      Node.ChildNodes[Comparison > 0] := N1;
  end
  else begin
    {Node has two branches; stack nodes to reach one with no right child}
    N1 := N2.ChildNodes[cALAVLBinaryTree_LeftChild];
    while Assigned(N1.ChildNodes[cALAVLBinaryTree_RightChild]) do begin
      Inc(StackPos);
      with Stack[StackPos] do begin
        Node := N1;
        Comparison := 1;
      end;
      N1 := N1.ChildNodes[cALAVLBinaryTree_RightChild];
    end;

    {Swap the node to delete with the terminal node}
    N1.Bal := N2.Bal;
    If StackParentPos = 0 then Fhead := N1
    else with Stack[StackParentPos] do
      Node.ChildNodes[Comparison > 0] := N1;

    with Stack[StackParentPos+1] do
      Node := N1;

    tmpnode := N1.ChildNodes[cALAVLBinaryTree_LeftChild];
    N1.ChildNodes[cALAVLBinaryTree_RightChild] := N2.ChildNodes[cALAVLBinaryTree_RightChild];
    N1.ChildNodes[cALAVLBinaryTree_LeftChild] := N2.ChildNodes[cALAVLBinaryTree_LeftChild];

    with Stack[StackPos] do
      Node.ChildNodes[Comparison > 0] := tmpnode;
  end;

  {return the deleted node}
  result := N2;
  Dec(FNodeCount);

  {Unwind the stack and rebalance}
  SubTreeDec := True;
  while (StackPos > 0) and SubTreeDec do begin
    if StackPos = 1 then AlAVLBinaryTree_DelBalance(Fhead, SubTreeDec, Stack[1].Comparison)
    else with Stack[StackPos-1] do
      AlAVLBinaryTree_DelBalance(Node.ChildNodes[Comparison > 0], SubTreeDec, Stack[StackPos].Comparison);
    dec(StackPos);
  end;
end;

{************************************************************************}
function TALBaseAVLBinaryTree.InternalDeleteNode(IdVal: Pointer): Boolean;
var N1: TALBaseAVLBinaryTreeNode;
begin
  N1 := InternalExtractNode(IdVal);
  if assigned(N1) then begin
    result := True;
    FreeNodeObj(N1);
  end
  else result := False;
end;

{**********************************************************************}
procedure TALBaseAVLBinaryTree.InternalLoadFromStream(Astream: Tstream);
Var K:Boolean;
    LstNode: TObjectStack;
    LParentNode, LNode: TALBaseAVLBinaryTreeNode;
begin
  {clear the binary tree}
  InternalClear;

  {create the TobjectStack}
  LstNode := TObjectStack.Create;
  Try

    {load the Head}
    AStream.Readbuffer(k, SizeOf(k));
    if k then begin
      FHead := CreateNode;
      AStream.ReadBuffer(FHead.Bal, SizeOf(FHead.Bal));
      FHead.LoadFromStream(aStream);
      inc(FnodeCount);
      FHead.childNodes[cALAVLBinaryTree_RightChild] := FHead; //a flag
      FHead.childNodes[cALAVLBinaryTree_LeftChild] := FHead;  //a flag
      {continue the loop with the leftchild and rightChild}
      LstNode.Push(FHead); //rightChild
      LstNode.Push(FHead); //leftChild
    end;

    {start the loop (if neccessary)}
    While LstNode.Count > 0 do begin

      {extract the parent node of the node where we will work on}
      LParentNode := TALBaseAVLBinaryTreeNode(LstNode.Pop);

      {load the data}
      AStream.ReadBuffer(k, SizeOf(k));
      if k then begin

        {find the good child node where we will work on}
        If LParentNode.childNodes[cALAVLBinaryTree_LeftChild] = LParentNode then begin
          LParentNode.childNodes[cALAVLBinaryTree_LeftChild] := CreateNode;
          LNode := LParentNode.childNodes[cALAVLBinaryTree_LeftChild];
        end
        else begin
          LParentNode.childNodes[cALAVLBinaryTree_RightChild] := CreateNode;
          LNode := LParentNode.childNodes[cALAVLBinaryTree_RightChild];
        end;

        AStream.ReadBuffer(LNode.Bal, SizeOf(LNode.Bal));
        LNode.LoadFromStream(aStream);
        inc(FnodeCount);
        LNode.childNodes[cALAVLBinaryTree_RightChild] := LNode; //a flag
        LNode.childNodes[cALAVLBinaryTree_LeftChild] := LNode;  //a flag
        {continue the loop with the leftchild and rightChild}
        LstNode.Push(LNode); //rightChild
        LstNode.Push(LNode); //leftChild
      end
      else begin
        {find the good child node where we will work on}
        If LParentNode.childNodes[cALAVLBinaryTree_LeftChild] = LParentNode then LParentNode.childNodes[cALAVLBinaryTree_LeftChild] := nil
        else LParentNode.childNodes[cALAVLBinaryTree_RightChild] := nil;
      end

    end;

  finally
    LstNode.free;
  end;
end;

{********************************************************************}
procedure TALBaseAVLBinaryTree.InternalSaveToStream(Astream: Tstream);
Var K:Boolean;
    LstNode: TObjectStack;
    LNode: TALBaseAVLBinaryTreeNode;
begin
  {create the TobjectStack}
  LstNode := TObjectStack.Create;
  Try

    {push the head in the TobjectStack}
    LstNode.Push(FHead);

    {start the loop}
    While LstNode.Count > 0 do begin
      LNode := TALBaseAVLBinaryTreeNode(LstNode.Pop);
      If assigned(LNode) then begin
        {write that the node exist}
        K := True;
        AStream.WriteBuffer(k, SizeOf(k));
        {write the balance}
        AStream.WriteBuffer(LNode.bal, SizeOf(LNode.bal));
        {write the data}
        LNode.SaveToStream(astream);
        {continue the loop with the leftchild and rightChild}
        LstNode.Push(LNode.childNodes[cALAVLBinaryTree_RightChild]);
        LstNode.Push(LNode.childNodes[cALAVLBinaryTree_LeftChild]);
      end
      else begin
        {write that the node doesn't exist}
        K := False;
        AStream.WriteBuffer(k, SizeOf(k));
      end;
    end;

  finally
    LstNode.free;
  end;
end;

{***********************************************************************}
procedure TALBaseAVLBinaryTree.LoadFromFile(const AFilename: AnsiString);
var LStream: TStream;
begin
  LStream := TFileStream.Create(String(aFileName), fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

{*********************************************************************}
procedure TALBaseAVLBinaryTree.SaveToFile(const AFilename: AnsiString);
var LStream: TStream;
begin
  LStream := TFileStream.Create(String(aFileName), fmCreate);
  try
    SaveToStream(LStream);
  finally
    LStream.Free;
  end;
end;

{***************************************************************************************}
function TALBaseAVLBinaryTree.InternalFindNode(idVal: pointer): TALBaseAVLBinaryTreeNode;
var N1: TALBaseAVLBinaryTreeNode;
    CmpRes: Integer;
begin
  N1 := FHead;
  while Assigned(N1) do begin
    CmpRes := CompareNode(IdVal, N1);
    if CmpRes = 0 then begin
      Result := N1;
      Exit;
    end
    else N1 := N1.ChildNodes[CmpRes > 0];
  end;

  Result := nil;
end;

{********************************************************************}
function TALBaseAVLBinaryTree.InternalFirst: TALBaseAVLBinaryTreeNode;
begin
  if FNodeCount = 0 then Result := nil
  else begin
    Result := Fhead;
    while Assigned(Result.ChildNodes[cALAVLBinaryTree_LeftChild]) do
      Result := Result.ChildNodes[cALAVLBinaryTree_LeftChild];
  end;
end;

{*******************************************************************}
function TALBaseAVLBinaryTree.InternalLast: TALBaseAVLBinaryTreeNode;
begin
  if FNodeCount = 0 then Result := nil
  else begin
    Result := FHead;
    while Assigned(Result.ChildNodes[cALAVLBinaryTree_RightChild]) do
      Result := Result.ChildNodes[cALAVLBinaryTree_RightChild];
  end;
end;

{****************************************************************************************************}
function TALBaseAVLBinaryTree.InternalNext(aNode: TALBaseAVLBinaryTreeNode): TALBaseAVLBinaryTreeNode;
var Found: Word;
    N1: TALBaseAVLBinaryTreeNode;
    StackPos: Integer;
    Stack: TALAVLBinaryTree_StackArray;
begin
  Result := nil;
  Found := 0;
  StackPos := 0;
  N1 := FHead;
  repeat
    while Assigned(N1) do begin
      Inc(StackPos);
      Stack[StackPos].Node := N1;
      N1 := N1.ChildNodes[cALAVLBinaryTree_LeftChild];
    end;
    if StackPos = 0 then Exit;

    N1 := Stack[StackPos].Node;
    Dec(StackPos);
    if Found = 1 then begin
      Result := N1;
      Exit;
    end;
    if N1 = aNode then Inc(Found);
    N1 := N1.ChildNodes[cALAVLBinaryTree_RightChild];
  until False;
end;

{****************************************************************************************************}
function TALBaseAVLBinaryTree.InternalPrev(aNode: TALBaseAVLBinaryTreeNode): TALBaseAVLBinaryTreeNode;
var Found: Word;
    N1: TALBaseAVLBinaryTreeNode;
    StackPos: Integer;
    Stack: TALAVLBinaryTree_StackArray;
begin
  Result := nil;
  Found := 0;
  StackPos := 0;
  N1 := FHead;
  repeat
    while Assigned(N1) do begin
      Inc(StackPos);
      Stack[StackPos].Node := N1;
      N1 := N1.ChildNodes[cALAVLBinaryTree_RightChild];
    end;
    if StackPos = 0 then Exit;

    N1 := Stack[StackPos].Node;
    Dec(StackPos);
    if Found = 1 then begin
      Result := N1;
      Exit;
    end;
    if N1 = aNode then
      Inc(Found);
    N1 := N1.ChildNodes[cALAVLBinaryTree_LeftChild];
  until False;
end;

{**********************************************************}
function TALBaseAVLBinaryTree.InternalGetNodeCount: integer;
begin
  Result := FnodeCount;
end;

{******************************************************************************}
function TALBaseAVLBinaryTree.AddNode(aNode: TALBaseAVLBinaryTreeNode): Boolean;
begin
  Result := InternalAddNode(aNode);
end;

{***********************************}
procedure TALBaseAVLBinaryTree.Clear;
begin
  InternalClear;
end;

{**********************************************************************************}
function TALBaseAVLBinaryTree.ExtractNode(IdVal: Pointer): TALBaseAVLBinaryTreeNode;
begin
  Result := InternalExtractNode(IdVal);
end;

{****************************************************************}
function TALBaseAVLBinaryTree.DeleteNode(IdVal: Pointer): Boolean;
begin
  Result := InternalDeleteNode(IdVal);
end;

{*******************************************************************************}
function TALBaseAVLBinaryTree.FindNode(idVal: pointer): TALBaseAVLBinaryTreeNode;
begin
  Result := InternalFindNode(idVal);
end;

{************************************************************}
function TALBaseAVLBinaryTree.First: TALBaseAVLBinaryTreeNode;
begin
  result := InternalFirst;
end;

{***********************************************************}
function TALBaseAVLBinaryTree.Head: TALBaseAVLBinaryTreeNode;
begin
  Result := InternalGetHead;
end;

{*********************************************************************************************************}
procedure TALBaseAVLBinaryTree.Iterate(Action: TALAVLBinaryTreeIterateFunc; Up: Boolean; ExtData: Pointer);
begin
  InternalIterate(Action, Up, ExtData);
end;

{***********************************************************}
function TALBaseAVLBinaryTree.Last: TALBaseAVLBinaryTreeNode;
begin
  Result := InternalLast;
end;

{********************************************************************************************}
function TALBaseAVLBinaryTree.Next(aNode: TALBaseAVLBinaryTreeNode): TALBaseAVLBinaryTreeNode;
begin
  Result := InternalNext(aNode);
end;

{********************************************************************************************}
function TALBaseAVLBinaryTree.Prev(aNode: TALBaseAVLBinaryTreeNode): TALBaseAVLBinaryTreeNode;
begin
  Result := InternalPrev(aNode);
end;

{************************************************************}
procedure TALBaseAVLBinaryTree.SaveToStream(Astream: Tstream);
begin
  InternalSaveToStream(Astream);
end;

{**************************************************************}
procedure TALBaseAVLBinaryTree.LoadFromStream(Astream: Tstream);
begin
  InternalLoadFromStream(Astream);
end;

{***********************************************}
function TALBaseAVLBinaryTree.NodeCount: integer;
begin
  Result := InternalGetNodeCount;
end;

{************************************************}
constructor TALIntegerKeyAVLBinaryTreeNode.Create;
begin
  Inherited;
  ID := 0;
end;

{************************************************************************}
procedure TALIntegerKeyAVLBinaryTreeNode.LoadFromStream(Astream: Tstream);
begin
  AStream.ReadBuffer(ID, SizeOf(ID));
end;

{**********************************************************************}
procedure TALIntegerKeyAVLBinaryTreeNode.SaveToStream(Astream: Tstream);
begin
  AStream.writeBuffer(ID, SizeOf(ID));
end;

{******************************************************************************************}
function TALIntegerKeyAVLBinaryTree.AddNode(aNode: TALIntegerKeyAVLBinaryTreeNode): Boolean;
begin
  Result := inherited addNode(aNode);
end;

{********************************************************************************************************}
function TALIntegerKeyAVLBinaryTree.CompareNode(IdVal: pointer; ANode: TALBaseAVLBinaryTreeNode): Integer;
Var LIntKey: Integer;
begin
  LIntKey := PInteger(IdVal)^;
  IF LIntKey = TALIntegerKeyAVLBinaryTreeNode(aNode).ID then result := 0
  else IF LIntKey > TALIntegerKeyAVLBinaryTreeNode(aNode).ID then result := 1
  else result := -1;
end;

{*************************************************************************************************}
function TALIntegerKeyAVLBinaryTree.CompareNode(aNode1, ANode2: TALBaseAVLBinaryTreeNode): Integer;
begin
  IF TALIntegerKeyAVLBinaryTreeNode(aNode1).ID = TALIntegerKeyAVLBinaryTreeNode(aNode2).ID then result := 0
  else IF TALIntegerKeyAVLBinaryTreeNode(aNode1).ID > TALIntegerKeyAVLBinaryTreeNode(aNode2).ID then result := 1
  else result := -1;
end;

{***********************************************************************}
function TALIntegerKeyAVLBinaryTree.CreateNode: TALBaseAVLBinaryTreeNode;
begin
  Result := TALIntegerKeyAVLBinaryTreeNode.Create;
end;

{**********************************************************************************************}
function TALIntegerKeyAVLBinaryTree.ExtractNode(IdVal: Integer): TALIntegerKeyAVLBinaryTreeNode;
begin
  result := TALIntegerKeyAVLBinaryTreeNode(inherited ExtractNode(@idVal));
end;

{**********************************************************************}
Function TALIntegerKeyAVLBinaryTree.DeleteNode(idVal: Integer): boolean;
begin
  result := inherited DeleteNode(@idVal);
end;

{*******************************************************************************************}
function TALIntegerKeyAVLBinaryTree.FindNode(idVal: Integer): TALIntegerKeyAVLBinaryTreeNode;
begin
  result := TALIntegerKeyAVLBinaryTreeNode(inherited FindNode(@idVal));
end;

{************************************************************************}
function TALIntegerKeyAVLBinaryTree.First: TALIntegerKeyAVLBinaryTreeNode;
begin
  Result := TALIntegerKeyAVLBinaryTreeNode(inherited First);
end;

{***********************************************************************}
function TALIntegerKeyAVLBinaryTree.Last: TALIntegerKeyAVLBinaryTreeNode;
begin
  Result := TALIntegerKeyAVLBinaryTreeNode(inherited Last);
end;

{**************************************************************************************************************}
function TALIntegerKeyAVLBinaryTree.Next(aNode: TALIntegerKeyAVLBinaryTreeNode): TALIntegerKeyAVLBinaryTreeNode;
begin
  Result := TALIntegerKeyAVLBinaryTreeNode(inherited Next(aNode));
end;

{**************************************************************************************************************}
function TALIntegerKeyAVLBinaryTree.Prev(aNode: TALIntegerKeyAVLBinaryTreeNode): TALIntegerKeyAVLBinaryTreeNode;
begin
  Result := TALIntegerKeyAVLBinaryTreeNode(inherited Prev(aNode));
end;

{***********************************************************************}
function TALIntegerKeyAVLBinaryTree.Head: TALIntegerKeyAVLBinaryTreeNode;
begin
  Result := TALIntegerKeyAVLBinaryTreeNode(inherited Head);
end;

{*************************************************}
constructor TALCardinalKeyAVLBinaryTreeNode.Create;
begin
  Inherited;
  ID := 0;
end;

{*************************************************************************}
procedure TALCardinalKeyAVLBinaryTreeNode.LoadFromStream(Astream: Tstream);
begin
  AStream.ReadBuffer(ID, SizeOf(ID));
end;

{***********************************************************************}
procedure TALCardinalKeyAVLBinaryTreeNode.SaveToStream(Astream: Tstream);
begin
  AStream.writeBuffer(ID, SizeOf(ID));
end;

{********************************************************************************************}
function TALCardinalKeyAVLBinaryTree.AddNode(aNode: TALCardinalKeyAVLBinaryTreeNode): Boolean;
begin
  Result := inherited addNode(aNode);
end;

{*********************************************************************************************************}
function TALCardinalKeyAVLBinaryTree.CompareNode(IdVal: pointer; ANode: TALBaseAVLBinaryTreeNode): Integer;
Var LCardKey: Cardinal;
begin
  LCardKey := PCardinal(IdVal)^;
  IF LCardKey = TALCardinalKeyAVLBinaryTreeNode(aNode).ID then result := 0
  else IF LCardKey > TALCardinalKeyAVLBinaryTreeNode(aNode).ID then result := 1
  else result := -1;
end;

{**************************************************************************************************}
function TALCardinalKeyAVLBinaryTree.CompareNode(aNode1, ANode2: TALBaseAVLBinaryTreeNode): Integer;
begin
  IF TALCardinalKeyAVLBinaryTreeNode(aNode1).ID = TALCardinalKeyAVLBinaryTreeNode(aNode2).ID then result := 0
  else IF TALCardinalKeyAVLBinaryTreeNode(aNode1).ID > TALCardinalKeyAVLBinaryTreeNode(aNode2).ID then result := 1
  else result := -1;
end;

{************************************************************************}
function TALCardinalKeyAVLBinaryTree.CreateNode: TALBaseAVLBinaryTreeNode;
begin
  Result := TALCardinalKeyAVLBinaryTreeNode.Create;
end;

{*************************************************************************************************}
function TALCardinalKeyAVLBinaryTree.ExtractNode(IdVal: Cardinal): TALCardinalKeyAVLBinaryTreeNode;
begin
  result := TALCardinalKeyAVLBinaryTreeNode(inherited ExtractNode(@idVal));
end;

{************************************************************************}
Function TALCardinalKeyAVLBinaryTree.DeleteNode(idVal: Cardinal): boolean;
begin
  result := inherited DeleteNode(@idVal);
end;

{**********************************************************************************************}
function TALCardinalKeyAVLBinaryTree.FindNode(idVal: Cardinal): TALCardinalKeyAVLBinaryTreeNode;
begin
  result := TALCardinalKeyAVLBinaryTreeNode(inherited FindNode(@idVal));
end;

{**************************************************************************}
function TALCardinalKeyAVLBinaryTree.First: TALCardinalKeyAVLBinaryTreeNode;
begin
  Result := TALCardinalKeyAVLBinaryTreeNode(inherited First);
end;

{*************************************************************************}
function TALCardinalKeyAVLBinaryTree.Last: TALCardinalKeyAVLBinaryTreeNode;
begin
  Result := TALCardinalKeyAVLBinaryTreeNode(inherited Last);
end;

{*****************************************************************************************************************}
function TALCardinalKeyAVLBinaryTree.Next(aNode: TALCardinalKeyAVLBinaryTreeNode): TALCardinalKeyAVLBinaryTreeNode;
begin
  Result := TALCardinalKeyAVLBinaryTreeNode(inherited Next(aNode));
end;

{*****************************************************************************************************************}
function TALCardinalKeyAVLBinaryTree.Prev(aNode: TALCardinalKeyAVLBinaryTreeNode): TALCardinalKeyAVLBinaryTreeNode;
begin
  Result := TALCardinalKeyAVLBinaryTreeNode(inherited Prev(aNode));
end;

{*************************************************************************}
function TALCardinalKeyAVLBinaryTree.Head: TALCardinalKeyAVLBinaryTreeNode;
begin
  Result := TALCardinalKeyAVLBinaryTreeNode(inherited Head);
end;

{**********************************************}
constructor TALInt64KeyAVLBinaryTreeNode.Create;
begin
  Inherited;
  ID := 0;
end;

{**********************************************************************}
procedure TALInt64KeyAVLBinaryTreeNode.LoadFromStream(Astream: Tstream);
begin
  AStream.ReadBuffer(ID, SizeOf(ID));
end;

{********************************************************************}
procedure TALInt64KeyAVLBinaryTreeNode.SaveToStream(Astream: Tstream);
begin
  AStream.writeBuffer(ID, SizeOf(ID));
end;

{**************************************************************************************}
function TALInt64KeyAVLBinaryTree.AddNode(aNode: TALInt64KeyAVLBinaryTreeNode): Boolean;
begin
  Result := inherited addNode(aNode);
end;

{******************************************************************************************************}
function TALInt64KeyAVLBinaryTree.CompareNode(IdVal: pointer; ANode: TALBaseAVLBinaryTreeNode): Integer;
Var LInt64Key: Int64;
begin
  LInt64Key := Pint64(IdVal)^;
  IF LInt64Key = TALInt64KeyAVLBinaryTreeNode(aNode).ID then result := 0
  else IF LInt64Key > TALInt64KeyAVLBinaryTreeNode(aNode).ID then result := 1
  else result := -1;
end;

{***********************************************************************************************}
function TALInt64KeyAVLBinaryTree.CompareNode(aNode1, ANode2: TALBaseAVLBinaryTreeNode): Integer;
begin
  IF TALint64KeyAVLBinaryTreeNode(aNode1).ID = TALint64KeyAVLBinaryTreeNode(aNode2).ID then result := 0
  else IF TALint64KeyAVLBinaryTreeNode(aNode1).ID > TALint64KeyAVLBinaryTreeNode(aNode2).ID then result := 1
  else result := -1;
end;

{*********************************************************************}
function TALInt64KeyAVLBinaryTree.CreateNode: TALBaseAVLBinaryTreeNode;
begin
  Result := TALint64KeyAVLBinaryTreeNode.Create;
end;

{****************************************************************************************}
function TALInt64KeyAVLBinaryTree.ExtractNode(IdVal: Int64): TALInt64KeyAVLBinaryTreeNode;
begin
  result := TALInt64KeyAVLBinaryTreeNode(inherited ExtractNode(@idVal));
end;

{******************************************************************}
function TALInt64KeyAVLBinaryTree.DeleteNode(idVal: Int64): boolean;
begin
  Result := inherited DeleteNode(@idVal);
end;

{*************************************************************************************}
function TALInt64KeyAVLBinaryTree.FindNode(idVal: Int64): TALInt64KeyAVLBinaryTreeNode;
begin
  Result := TALInt64KeyAVLBinaryTreeNode(inherited FindNode(@idVal));
end;

{********************************************************************}
function TALInt64KeyAVLBinaryTree.First: TALInt64KeyAVLBinaryTreeNode;
begin
  Result := TALInt64KeyAVLBinaryTreeNode(inherited First);
end;

{*******************************************************************}
function TALInt64KeyAVLBinaryTree.Last: TALInt64KeyAVLBinaryTreeNode;
begin
  Result := TALInt64KeyAVLBinaryTreeNode(inherited Last);
end;

{********************************************************************************************************}
function TALInt64KeyAVLBinaryTree.Next(aNode: TALInt64KeyAVLBinaryTreeNode): TALInt64KeyAVLBinaryTreeNode;
begin
  Result := TALInt64KeyAVLBinaryTreeNode(inherited Next(aNode));
end;

{********************************************************************************************************}
function TALInt64KeyAVLBinaryTree.Prev(aNode: TALInt64KeyAVLBinaryTreeNode): TALInt64KeyAVLBinaryTreeNode;
begin
  Result := TALInt64KeyAVLBinaryTreeNode(inherited Prev(aNode));
end;

{*******************************************************************}
function TALInt64KeyAVLBinaryTree.Head: TALInt64KeyAVLBinaryTreeNode;
begin
  Result := TALInt64KeyAVLBinaryTreeNode(inherited Head);
end;

{***********************************************}
constructor TALStringKeyAVLBinaryTreeNode.Create;
begin
  inherited;
  ID := '';
end;

{***********************************************************************}
procedure TALStringKeyAVLBinaryTreeNode.LoadFromStream(Astream: Tstream);
Var K:integer;
begin
  AStream.ReadBuffer(k, SizeOf(k));
  SetLength(ID, k);
  if k > 0 then AStream.ReadBuffer(pointer(ID)^, k);
end;

{*********************************************************************}
procedure TALStringKeyAVLBinaryTreeNode.SaveToStream(Astream: Tstream);
Var K:integer;
begin
  K := length(ID);
  AStream.writeBuffer(k, SizeOf(k));
  if k > 0 then AStream.writeBuffer(pointer(ID)^, k);
end;

{*******************************************}
constructor TALStringKeyAVLBinaryTree.Create;
begin
  inherited;
  FcaseSensitive := True;
  FcompareKeyFunct := CompareKeyCaseSensitive;
end;

{**********************************************************************}
function TALStringKeyAVLBinaryTree.CreateNode: TALBaseAVLBinaryTreeNode;
begin
  Result := TALStringKeyAVLBinaryTreeNode.Create;
end;

{****************************************************************************************}
function TALStringKeyAVLBinaryTree.AddNode(aNode: TALStringKeyAVLBinaryTreeNode): Boolean;
begin
  Result := inherited addNode(aNode);
end;

{*******************************************************************************************************}
function TALStringKeyAVLBinaryTree.CompareNode(IdVal: pointer; ANode: TALBaseAVLBinaryTreeNode): Integer;
begin
  Result := FcompareKeyFunct(PAnsiString(IdVal)^,TALStringKeyAVLBinaryTreeNode(aNode).ID);
end;

{***********************************************************************************************}
function TALStringKeyAVLBinaryTree.CompareNode(aNode1,ANode2: TALBaseAVLBinaryTreeNode): Integer;
begin
  Result := FcompareKeyFunct(TALStringKeyAVLBinaryTreeNode(aNode1).ID,TALStringKeyAVLBinaryTreeNode(aNode2).ID);
end;

{*****************************************************************************************************}
function TALStringKeyAVLBinaryTree.ExtractNode(const IdVal: AnsiString): TALStringKeyAVLBinaryTreeNode;
begin
  result := TALStringKeyAVLBinaryTreeNode(inherited ExtractNode(@idVal));
end;

{******************************************************************************}
function TALStringKeyAVLBinaryTree.DeleteNode(const idVal: AnsiString): boolean;
begin
  result := inherited DeleteNode(@idVal);
end;

{**************************************************************************************************}
function TALStringKeyAVLBinaryTree.FindNode(const idVal: AnsiString): TALStringKeyAVLBinaryTreeNode;
begin
  Result := TALStringKeyAVLBinaryTreeNode(inherited FindNode(@idVal));
end;

{**********************************************************************}
function TALStringKeyAVLBinaryTree.First: TALStringKeyAVLBinaryTreeNode;
begin
  Result := TALStringKeyAVLBinaryTreeNode(inherited First);
end;

{*********************************************************************}
function TALStringKeyAVLBinaryTree.Last: TALStringKeyAVLBinaryTreeNode;
begin
  Result := TALStringKeyAVLBinaryTreeNode(inherited Last);
end;

{***********************************************************************************************************}
function TALStringKeyAVLBinaryTree.Next(aNode: TALStringKeyAVLBinaryTreeNode): TALStringKeyAVLBinaryTreeNode;
begin
  Result := TALStringKeyAVLBinaryTreeNode(inherited Next(aNode));
end;

{***********************************************************************************************************}
function TALStringKeyAVLBinaryTree.Prev(aNode: TALStringKeyAVLBinaryTreeNode): TALStringKeyAVLBinaryTreeNode;
begin
  Result := TALStringKeyAVLBinaryTreeNode(inherited Prev(aNode));
end;


{****************************************************************************************************}
function TALStringKeyAVLBinaryTree.CompareKeyCaseInSensitive(const aKey1, aKey2: AnsiString): Integer;
begin
  Result := ALCompareText(aKey1,aKey2);
end;

{**************************************************************************************************}
function TALStringKeyAVLBinaryTree.CompareKeyCaseSensitive(const aKey1, aKey2: AnsiString): Integer;
begin
  result := ALCompareStr(aKey1,aKey2);
end;

{*************************************************************************}
procedure TALStringKeyAVLBinaryTree.SetcaseSensitive(const Value: Boolean);
begin
  If Value <> FCaseSensitive then begin
    FCaseSensitive := Value;
    If FCaseSensitive then FcompareKeyFunct := CompareKeyCaseSensitive
    else FcompareKeyFunct := CompareKeyCaseInSensitive;
  end;
end;

{*********************************************************************}
function TALStringKeyAVLBinaryTree.Head: TALStringKeyAVLBinaryTreeNode;
begin
  Result := TALStringKeyAVLBinaryTreeNode(inherited head);
end;

end.
