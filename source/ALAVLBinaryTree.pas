{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALAVLBinaryTree (Self-Balancing Binary Trees)
Version:      3.50

Description:  - These binary trees are self-balancing in the AVL sense
                (the depth of any left branch differs by no more than
                one from the depth of the right branch).

              - Duplicate data is not allowed in a tree.

              - Nodes can be of type TALBaseAVLBinaryTreeNode or any
                descendant.

              - Next and Prev should not be used to iterate through an
                entire tree. This is much slower than calling the Iterate
                method.

Legal issues: Copyright (C) 1999-2009 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History :     27/10/2005: Rebuild of the unit
              17/11/2005: improuve the nodecount property
              17/12/2005: add savetostream and loadfromstream
              10/01/2006: rebuild the unit to add AVL support
                          rename unit from ALBinaryTree to
                          ALAVLBinaryTree;
              25/02/2006: Update TALCardinalKeySessionAVLBinaryTree.CreateSessionNode
                          to delete expired node;
              04/04/2006: add savetostream and loadfromstream to all descendant of TALBaseAVLBinaryTreeNode
              04/04/2006: add TALIntegerKeyAVLBinaryTree
              01/08/2006: add FindAndAcquireNode, AcquireNode, FindAndReleaseNode, ReleaseNode,
                          CreateAndAcquireSessionNode to TALCardinalKeySessionAVLBinaryTree.
              01/07/2008: add the procedure rebuild (when we add the value already ordered in the bintree,
                          we have the worse performance ! rebuild can correct this problem)

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALAVLBinaryTree;

interface

uses classes,
     sysUtils,
     syncObjs;

type

  {class defintion----------------}
  TALBaseAVLBinaryTreeNode = class;
  TALBaseAVLBinaryTree = class;

  {iterate function----------------------}
  TALAVLBinaryTreeIterateFunc = procedure(
                                          aTree: TALBaseAVLBinaryTree;
                                          aNode: TALBaseAVLBinaryTreeNode;
                                          aExtData: Pointer;
                                          Var aContinue: Boolean
                                         );



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
    Function    DeleteNode(IdVal: Pointer): Boolean; virtual;
    Procedure   Clear; Virtual;
    Function    Head: TALBaseAVLBinaryTreeNode; virtual;
    Procedure   SaveToStream(Astream: Tstream); Virtual;
    Procedure   LoadFromStream(Astream: Tstream); Virtual;
    Procedure   SaveToFile(AFilename: String); Virtual;
    Procedure   LoadFromFile(AFilename: String); Virtual;
    function    FindNode(idVal: pointer): TALBaseAVLBinaryTreeNode; virtual;
    function    First: TALBaseAVLBinaryTreeNode; virtual; {Return the smallest-value node in the tree}
    function    Last: TALBaseAVLBinaryTreeNode; virtual; {Return the largest-value node in the tree}
    function    Next(aNode: TALBaseAVLBinaryTreeNode): TALBaseAVLBinaryTreeNode; virtual; {Return the next node whose value is larger than aNode}
    function    Prev(aNode: TALBaseAVLBinaryTreeNode): TALBaseAVLBinaryTreeNode; virtual; {Return the largest node whose value is smaller than aNode}
    Function    NodeCount: integer; virtual;
    Procedure   Rebuild;
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
    ID: String;
    Constructor Create; Override;
  end;

  {TALStringKeyAVLBinaryTreeCompareKeyFunct----------------------------------------------------------}
  TALStringKeyAVLBinaryTreeCompareKeyFunct = function (const aKey1, aKey2: String): Integer of object;

  {TALStringKeyAVLBinaryTree----------------------------}
  TALStringKeyAVLBinaryTree = class(TALBaseAVLBinaryTree)
  private
    FCaseSensitive: Boolean;
    FcompareKeyFunct: TALStringKeyAVLBinaryTreeCompareKeyFunct;
    procedure SetcaseSensitive(const Value: Boolean);
  protected
    Function CompareKeyCaseSensitive(Const aKey1, aKey2: String): Integer; {compares akey1 and akey2 and returns 0 if they are equal. If akey1 is greater than akey2, returns an integer greater than 0. If akey1 is less than akey2, returns an integer less than 0.}
    Function CompareKeyCaseInSensitive(Const aKey1, aKey2: String): Integer; {compares akey1 and akey2 and returns 0 if they are equal. If akey1 is greater than akey2, returns an integer greater than 0. If akey1 is less than akey2, returns an integer less than 0.}
    Function CompareNode(IdVal: pointer; ANode: TALBaseAVLBinaryTreeNode): Integer; override; {compares IdVal and aNode.keydata and returns 0 if they are equal. If IdVal is greater than aNode.keydata, returns an integer greater than 0. If aNode.keydata is less than IdVal, returns an integer less than 0.}
    Function CompareNode(aNode1, ANode2: TALBaseAVLBinaryTreeNode): Integer; override; {compares aNode1 and aNode2 and returns 0 if they are equal. If aNode1 is greater than aNode2, returns an integer greater than 0. If aNode1 is less than aNode2, returns an integer less than 0.}
    function CreateNode: TALBaseAVLBinaryTreeNode; override;
  public
    Constructor Create; override;
    function    AddNode(aNode: TALStringKeyAVLBinaryTreeNode): Boolean; reintroduce; virtual;
    function    DeleteNode(idVal: String): boolean; reintroduce; virtual;
    Function    Head: TALStringKeyAVLBinaryTreeNode; Reintroduce; virtual;
    function    FindNode(idVal: String): TALStringKeyAVLBinaryTreeNode; Reintroduce; virtual;
    function    First: TALStringKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the smallest-value node in the tree}
    function    Last: TALStringKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest-value node in the tree}
    function    Next(aNode: TALStringKeyAVLBinaryTreeNode): TALStringKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the next node whose value is larger than aNode}
    function    Prev(aNode: TALStringKeyAVLBinaryTreeNode): TALStringKeyAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest node whose value is smaller than aNode}
    Property    CaseSensitive: Boolean read FCaseSensitive write SetcaseSensitive default True;
  end;



  {TALCardinalKeySessionAVLBinaryTreeNode---------------------------------------}
  TALCardinalKeySessionAVLBinaryTreeNode = class(TALCardinalKeyAVLBinaryTreeNode)
  Private
  Protected
    Procedure   SaveToStream(Astream: Tstream); override;
    Procedure   LoadFromStream(Astream: Tstream); override;
  Public
    Acquired: Boolean;
    LastAccess: TDateTime;
    Data: TStringList;
    Constructor Create; override;
    Destructor  Destroy; override;
  end;

  {TALCardinalKeySessionAVLBinaryTree-----------------------------------}
  TALCardinalKeySessionAVLBinaryTree = class(TALCardinalKeyAVLBinaryTree)
  private
    FcriticalSection: TcriticalSection;
    FLastCheck: TDateTime;
    FCheckInterval: TDateTime;
    FExpireInterval: TDateTime;
  protected
    procedure DeleteExpiredNode; overload; virtual;
    function  CreateNode: TALBaseAVLBinaryTreeNode; override;
  public
    Constructor Create; override;
    Destructor  Destroy; override;
    function    CreateSessionNode: TALCardinalKeySessionAVLBinaryTreeNode; virtual;
    procedure   Iterate(Action: TALAVLBinaryTreeIterateFunc; Up: Boolean; ExtData: Pointer); override;
    function    AddNode(aNode: TALCardinalKeySessionAVLBinaryTreeNode): Boolean; reintroduce; virtual;
    function    DeleteNode(idVal: Cardinal): boolean; override;
    Function    Head: TALCardinalKeySessionAVLBinaryTreeNode; Reintroduce; virtual;
    function    FindNode(idVal: Cardinal): TALCardinalKeySessionAVLBinaryTreeNode; Reintroduce; virtual;
    function    First: TALCardinalKeySessionAVLBinaryTreeNode; Reintroduce; virtual; {Return the smallest-value node in the tree}
    function    Last: TALCardinalKeySessionAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest-value node in the tree}
    function    Next(aNode: TALCardinalKeySessionAVLBinaryTreeNode): TALCardinalKeySessionAVLBinaryTreeNode; Reintroduce; virtual; {Return the next node whose value is larger than aNode}
    function    Prev(aNode: TALCardinalKeySessionAVLBinaryTreeNode): TALCardinalKeySessionAVLBinaryTreeNode; Reintroduce; virtual; {Return the largest node whose value is smaller than aNode}
    Procedure   Clear; override;
    Property    CheckInterval: TDateTime read FcheckInterval Write FcheckInterval;
    Property    ExpireInterval: TDateTime read FExpireInterval Write FExpireInterval;
    Procedure   LoadFromStream(Astream: Tstream); Override;
    Procedure   SaveToStream(Astream: Tstream); override;
    Function    NodeCount: integer; override;
    function    FindAndAcquireNode(idVal: Cardinal): TALCardinalKeySessionAVLBinaryTreeNode; virtual;
    procedure   AcquireNode(aNode: TALCardinalKeySessionAVLBinaryTreeNode); virtual;
    function    FindAndReleaseNode(idVal: Cardinal): TALCardinalKeySessionAVLBinaryTreeNode; virtual;
    procedure   ReleaseNode(aNode: TALCardinalKeySessionAVLBinaryTreeNode); virtual;
    function    CreateAndAcquireSessionNode: TALCardinalKeySessionAVLBinaryTreeNode; virtual;

  end;

implementation

uses Contnrs;

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




//////////////////////////////////////////////
////////// TALBaseAVLBinaryTreeNode //////////
//////////////////////////////////////////////

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




///////////////////////////////////////
////////// TALBaseAVLBinaryTree //////////
///////////////////////////////////////

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
  InternalIterate(
                  AlAVLBinaryTree_IterateDestroyNodeFunc,
                  True,
                  nil
                 );
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

{************************************************************************}
function TALBaseAVLBinaryTree.InternalDeleteNode(IdVal: Pointer): Boolean;
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
    result := False;
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
        Result := False;
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

  {Dispose of the deleted node}
  FreeNodeObj(N2);
  Dec(FNodeCount);
  Result := True;

  {Unwind the stack and rebalance}
  SubTreeDec := True;
  while (StackPos > 0) and SubTreeDec do begin
    if StackPos = 1 then AlAVLBinaryTree_DelBalance(Fhead, SubTreeDec, Stack[1].Comparison)
    else with Stack[StackPos-1] do
      AlAVLBinaryTree_DelBalance(Node.ChildNodes[Comparison > 0], SubTreeDec, Stack[StackPos].Comparison);
    dec(StackPos);
  end;
end;

{**********************************************************************}
procedure TALBaseAVLBinaryTree.InternalLoadFromStream(Astream: Tstream);
Var K:Boolean;
    LstNode: TObjectStack;
    aParentNode, aNode: TALBaseAVLBinaryTreeNode;
begin
  {clear the binary tree}
  InternalClear;

  {create the TobjectStack}
  LstNode := TObjectStack.Create;
  Try

    {load the Head}
    AStream.Read(k, SizeOf(k));
    if k then begin
      FHead := CreateNode;
      AStream.Read(FHead.Bal, SizeOf(FHead.Bal));
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
      aParentNode := TALBaseAVLBinaryTreeNode(LstNode.Pop);

      {load the data}
      AStream.Read(k, SizeOf(k));
      if k then begin

        {find the good child node where we will work on}
        If aParentNode.childNodes[cALAVLBinaryTree_LeftChild] = aParentNode then begin
          aParentNode.childNodes[cALAVLBinaryTree_LeftChild] := CreateNode;
          aNode := aParentNode.childNodes[cALAVLBinaryTree_LeftChild];
        end
        else begin
          aParentNode.childNodes[cALAVLBinaryTree_RightChild] := CreateNode;
          aNode := aParentNode.childNodes[cALAVLBinaryTree_RightChild];
        end;

        AStream.Read(ANode.Bal, SizeOf(ANode.Bal));
        ANode.LoadFromStream(aStream);
        inc(FnodeCount);
        aNode.childNodes[cALAVLBinaryTree_RightChild] := aNode; //a flag
        aNode.childNodes[cALAVLBinaryTree_LeftChild] := aNode;  //a flag
        {continue the loop with the leftchild and rightChild}
        LstNode.Push(aNode); //rightChild
        LstNode.Push(aNode); //leftChild
      end
      else begin
        {find the good child node where we will work on}
        If aParentNode.childNodes[cALAVLBinaryTree_LeftChild] = aParentNode then aParentNode.childNodes[cALAVLBinaryTree_LeftChild] := nil
        else aParentNode.childNodes[cALAVLBinaryTree_RightChild] := nil;
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
    aNode: TALBaseAVLBinaryTreeNode;
begin
  {create the TobjectStack}
  LstNode := TObjectStack.Create;
  Try

    {push the head in the TobjectStack}
    LstNode.Push(FHead);

    {start the loop}
    While LstNode.Count > 0 do begin
      aNode := TALBaseAVLBinaryTreeNode(LstNode.Pop);
      If assigned(aNode) then begin
        {write that the node exist}
        K := True;
        AStream.Write(k, SizeOf(k));
        {write the balance}
        AStream.Write(aNode.bal, SizeOf(aNode.bal));
        {write the data}
        Anode.SaveToStream(astream);
        {continue the loop with the leftchild and rightChild}
        LstNode.Push(aNode.childNodes[cALAVLBinaryTree_RightChild]);
        LstNode.Push(aNode.childNodes[cALAVLBinaryTree_LeftChild]);
      end
      else begin
        {write that the node doesn't exist}
        K := False;
        AStream.Write(k, SizeOf(k));
      end;
    end;

  finally
    LstNode.free;
  end;
end;

{*************************************************************}
procedure TALBaseAVLBinaryTree.LoadFromFile(AFilename: String);
var aStream: TStream;
begin
  aStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(aStream);
  finally
    aStream.Free;
  end;
end;

{***********************************************************}
procedure TALBaseAVLBinaryTree.SaveToFile(AFilename: String);
var aStream: TStream;
begin
  aStream := TFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(aStream);
  finally
    aStream.Free;
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

{*************************************}
procedure TALBaseAVLBinaryTree.Rebuild;
Var aObjectStack: TObjectStack;
    aObjectList: TObjectList;
    aNode: TALBaseAVLBinaryTreeNode;
    i: integer;
begin

  {create the TobjectStack}
  aObjectStack := TObjectStack.Create;
  Try

    {create the aObjectList}
    aObjectList := TobjectList.Create(False);
    Try

      {push the head in the TobjectStack}
      aObjectStack.Push(Head);

      {start the loop}
      While aObjectStack.Count > 0 do begin
        aNode := TALBaseAVLBinaryTreeNode(aObjectStack.Pop);
        If assigned(aNode) then begin
          {add the Node to the List}
          aObjectList.Add(aNode);
          {continue the loop with the leftchild and rightChild}
          aObjectStack.Push(aNode.ChildNodes[True]);
          aObjectStack.Push(aNode.ChildNodes[False]);
        end;
      end;

      {clear the BinTree}
      FHead := nil;
      FNodeCount := 0;

      {add randomly the node in the bintree}
      While aObjectList.Count > 0 do begin
        i := Random(aObjectList.Count);
        aNode := TALBaseAVLBinaryTreeNode(aObjectList[i]);
        aObjectList.Delete(i);
        aNode.ChildNodes[true] := nil;
        aNode.ChildNodes[False] := nil;
        aNode.Bal := 0;
        addNode(aNode);
      end;

    Finally
      aObjectList.Free;
    End;

  finally
    aObjectStack.free;
  end;

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



////////////////////////////////////////////////////
////////// TALIntegerKeyAVLBinaryTreeNode //////////
////////////////////////////////////////////////////

{************************************************}
constructor TALIntegerKeyAVLBinaryTreeNode.Create;
begin
  Inherited;
  ID := 0;
end;

{************************************************************************}
procedure TALIntegerKeyAVLBinaryTreeNode.LoadFromStream(Astream: Tstream);
begin
  AStream.Read(ID, SizeOf(ID));
end;

{**********************************************************************}
procedure TALIntegerKeyAVLBinaryTreeNode.SaveToStream(Astream: Tstream);
begin
  AStream.write(ID, SizeOf(ID));
end;



/////////////////////////////////////////////////
////////// TALIntegerKeyAVLBinaryTree //////////
/////////////////////////////////////////////////

{******************************************************************************************}
function TALIntegerKeyAVLBinaryTree.AddNode(aNode: TALIntegerKeyAVLBinaryTreeNode): Boolean;
begin
  Result := inherited addNode(aNode);
end;

{********************************************************************************************************}
function TALIntegerKeyAVLBinaryTree.CompareNode(IdVal: pointer; ANode: TALBaseAVLBinaryTreeNode): Integer;
Var aIntKey: Integer;
begin
  aIntKey := PInteger(IdVal)^;
  IF aIntKey = TALIntegerKeyAVLBinaryTreeNode(aNode).ID then result := 0
  else IF aIntKey > TALIntegerKeyAVLBinaryTreeNode(aNode).ID then result := 1
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




/////////////////////////////////////////////////////
////////// TALCardinalKeyAVLBinaryTreeNode //////////
/////////////////////////////////////////////////////

{*************************************************}
constructor TALCardinalKeyAVLBinaryTreeNode.Create;
begin
  Inherited;
  ID := 0;
end;

{*************************************************************************}
procedure TALCardinalKeyAVLBinaryTreeNode.LoadFromStream(Astream: Tstream);
begin
  AStream.Read(ID, SizeOf(ID));
end;

{***********************************************************************}
procedure TALCardinalKeyAVLBinaryTreeNode.SaveToStream(Astream: Tstream);
begin
  AStream.write(ID, SizeOf(ID));
end;



/////////////////////////////////////////////////
////////// TALCardinalKeyAVLBinaryTree //////////
/////////////////////////////////////////////////

{********************************************************************************************}
function TALCardinalKeyAVLBinaryTree.AddNode(aNode: TALCardinalKeyAVLBinaryTreeNode): Boolean;
begin
  Result := inherited addNode(aNode);
end;

{*********************************************************************************************************}
function TALCardinalKeyAVLBinaryTree.CompareNode(IdVal: pointer; ANode: TALBaseAVLBinaryTreeNode): Integer;
Var aCardKey: Cardinal;
begin
  aCardKey := PCardinal(IdVal)^;
  IF aCardKey = TALCardinalKeyAVLBinaryTreeNode(aNode).ID then result := 0
  else IF aCardKey > TALCardinalKeyAVLBinaryTreeNode(aNode).ID then result := 1
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




//////////////////////////////////////////////////
////////// TALInt64KeyAVLBinaryTreeNode //////////
//////////////////////////////////////////////////

{**********************************************}
constructor TALInt64KeyAVLBinaryTreeNode.Create;
begin
  Inherited;
  ID := 0;
end;

{**********************************************************************}
procedure TALInt64KeyAVLBinaryTreeNode.LoadFromStream(Astream: Tstream);
begin
  AStream.Read(ID, SizeOf(ID));
end;

{********************************************************************}
procedure TALInt64KeyAVLBinaryTreeNode.SaveToStream(Astream: Tstream);
begin
  AStream.write(ID, SizeOf(ID));
end;



//////////////////////////////////////////////
////////// TALInt64KeyAVLBinaryTree //////////
//////////////////////////////////////////////

{**************************************************************************************}
function TALInt64KeyAVLBinaryTree.AddNode(aNode: TALInt64KeyAVLBinaryTreeNode): Boolean;
begin
  Result := inherited addNode(aNode);
end;

{******************************************************************************************************}
function TALInt64KeyAVLBinaryTree.CompareNode(IdVal: pointer; ANode: TALBaseAVLBinaryTreeNode): Integer;
Var aInt64Key: Int64;
begin
  aInt64Key := Pint64(IdVal)^;
  IF aInt64Key = TALInt64KeyAVLBinaryTreeNode(aNode).ID then result := 0
  else IF aInt64Key > TALInt64KeyAVLBinaryTreeNode(aNode).ID then result := 1
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




///////////////////////////////////////////////////
////////// TALStringKeyAVLBinaryTreeNode //////////
///////////////////////////////////////////////////

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
  AStream.Read(k, SizeOf(k));
  SetLength(ID, k);
  if k > 0 then AStream.Read(ID[1], k);
end;

{*********************************************************************}
procedure TALStringKeyAVLBinaryTreeNode.SaveToStream(Astream: Tstream);
Var K:integer;
begin
  K := length(ID);
  AStream.write(k, SizeOf(k));
  if k > 0 then AStream.write(ID[1], k);
end;




///////////////////////////////////////////////
////////// TALStringKeyAVLBinaryTree //////////
///////////////////////////////////////////////

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
  Result := FcompareKeyFunct(PString(IdVal)^,TALStringKeyAVLBinaryTreeNode(aNode).ID);
end;

{***********************************************************************************************}
function TALStringKeyAVLBinaryTree.CompareNode(aNode1,ANode2: TALBaseAVLBinaryTreeNode): Integer;
begin
  Result := FcompareKeyFunct(TALStringKeyAVLBinaryTreeNode(aNode1).ID,TALStringKeyAVLBinaryTreeNode(aNode2).ID);
end;

{********************************************************************}
function TALStringKeyAVLBinaryTree.DeleteNode(idVal: String): boolean;
begin
  result := inherited DeleteNode(@idVal);
end;

{****************************************************************************************}
function TALStringKeyAVLBinaryTree.FindNode(idVal: String): TALStringKeyAVLBinaryTreeNode;
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


{************************************************************************************************}
function TALStringKeyAVLBinaryTree.CompareKeyCaseInSensitive(const aKey1, aKey2: String): Integer;
begin
  Result := CompareText(aKey1,aKey2);
end;

{**********************************************************************************************}
function TALStringKeyAVLBinaryTree.CompareKeyCaseSensitive(const aKey1, aKey2: String): Integer;
begin
  result := CompareStr(aKey1,aKey2);
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




////////////////////////////////////////////////////////////
////////// TALCardinalKeySessionAVLBinaryTreeNode //////////
////////////////////////////////////////////////////////////

{********************************************************}
constructor TALCardinalKeySessionAVLBinaryTreeNode.Create;
begin
  inherited;
  Acquired := False;
  LastAccess := Now;
  Data := TStringList.Create;
  Data.Delimiter := ';';
  Data.QuoteChar := '''';
end;

{********************************************************}
destructor TALCardinalKeySessionAVLBinaryTreeNode.Destroy;
begin
  Data.free;
  inherited;
end;

{********************************************************************************}
procedure TALCardinalKeySessionAVLBinaryTreeNode.LoadFromStream(Astream: Tstream);
Var K:integer;
    str: String;
begin
  inherited;
  AStream.Read(LastAccess, SizeOf(LastAccess));
  AStream.Read(k, SizeOf(k));
  if k > 0 then begin
    SetLength(str, k);
    AStream.Read(str[1], k);
    Data.DelimitedText := Str;
  end;
end;

{******************************************************************************}
procedure TALCardinalKeySessionAVLBinaryTreeNode.SaveToStream(Astream: Tstream);
Var K:integer;
    str: String;
begin
  inherited;
  AStream.write(LastAccess, SizeOf(LastAccess));
  Str := Data.DelimitedText;
  K := length(str);
  AStream.write(k, SizeOf(k));
  if k > 0 then AStream.write(str[1], k);
end;




////////////////////////////////////////////////////////
////////// TALCardinalKeySessionAVLBinaryTree //////////
////////////////////////////////////////////////////////

{**}
Type
  TAlAVLBinaryTree_IterateDeleteExpiredNodeExtData = record
    lstExpiredNode : TList;
    LowDateTime: TdateTime;
  end;

{*****************************************************************************}
procedure AlAVLBinaryTree_IterateDeleteExpiredNode(aTree: TALBaseAVLBinaryTree;
                                                   aNode: TALBaseAVLBinaryTreeNode;
                                                   aExtData: Pointer;
                                                   Var aContinue: Boolean);

begin
  With TAlAVLBinaryTree_IterateDeleteExpiredNodeExtData(aExtData^) do
    if (TALCardinalKeySessionAVLBinaryTreeNode(aNode).LastAccess < LowDateTime) then lstExpiredNode.add(aNode);

  acontinue := True;
end;

{****************************************************}
constructor TALCardinalKeySessionAVLBinaryTree.Create;
begin
  FcriticalSection := TcriticalSection.create;
  FCheckInterval := 0.0021; // Every three minutes ( 1 / 24 / 60 * 3)
  FExpireInterval := 0.0834; // 120 minutes ( 1 / 24 / 60 * 120)
  FLastCheck := Now;
  Randomize;
  Inherited;
end;

{****************************************************}
destructor TALCardinalKeySessionAVLBinaryTree.Destroy;
begin
  FcriticalSection.Free;
  inherited;
end;

{*******************************************************************************}
function TALCardinalKeySessionAVLBinaryTree.CreateNode: TALBaseAVLBinaryTreeNode;
begin
  Result := TALCardinalKeySessionAVLBinaryTreeNode.create;
end;

{****************************************************************************************************}
function TALCardinalKeySessionAVLBinaryTree.CreateSessionNode: TALCardinalKeySessionAVLBinaryTreeNode;
var aNode: TALCardinalKeySessionAVLBinaryTreeNode;
begin
  FcriticalSection.Acquire;
  Try

    if (Now - FLastCheck > FCheckInterval) then begin
      DeleteExpiredNode;
      FLastCheck := Now;
    end;

    aNode := TALCardinalKeySessionAVLBinaryTreeNode(CreateNode);
    aNode.ID := Random(MaxInt) + Random(MaxInt);
    while (not InternalAddNode(aNode)) do aNode.ID := Random(MaxInt) + Random(MaxInt);
    result := aNode;

  Finally
    FcriticalSection.Release;
  end;
end;

{**********************************************************************************************************}
function TALCardinalKeySessionAVLBinaryTree.AddNode(aNode: TALCardinalKeySessionAVLBinaryTreeNode): Boolean;
begin
  FcriticalSection.Acquire;
  Try

    if (Now - FLastCheck > FCheckInterval) then begin
      DeleteExpiredNode;
      FLastCheck := Now;
    end;

    result := inherited AddNode(aNode);

  Finally
    FcriticalSection.Release;
  end;
end;

{************************************************************************************************************}
function TALCardinalKeySessionAVLBinaryTree.FindNode(idVal: Cardinal): TALCardinalKeySessionAVLBinaryTreeNode;
begin
  FcriticalSection.Acquire;
  try

    if (Now - FLastCheck > FCheckInterval) then begin
      DeleteExpiredNode;
      FLastCheck := Now;
    end;

    result := TALCardinalKeySessionAVLBinaryTreeNode(inherited FindNode(idVal));
    If result <> nil then result.LastAccess := now;

  finally
    FcriticalSection.Release;
  end;
end;

{*******************************************************************************}
function TALCardinalKeySessionAVLBinaryTree.DeleteNode(idVal: Cardinal): boolean;
begin
  FcriticalSection.Acquire;
  try

    result := inherited DeleteNode(IdVal);

  finally
    FcriticalSection.Release;
  end;
end;

{*************************************************************}
procedure TALCardinalKeySessionAVLBinaryTree.DeleteExpiredNode;
Var aExtData: TAlAVLBinaryTree_IterateDeleteExpiredNodeExtData;
    i: integer;
begin
  aExtData.lstExpiredNode := Tlist.Create;
  Try
    aExtData.LowDateTime := Now - FExpireInterval;

    InternalIterate(
                    AlAVLBinaryTree_IterateDeleteExpiredNode,
                    True,
                    @aExtData
                   );

    for i := 0 to aExtData.lstExpiredNode.Count - 1 do
      InternalDeleteNode(@TALCardinalKeySessionAVLBinaryTreeNode(aExtData.lstExpiredNode[i]).ID);

  finally
    aExtData.lstExpiredNode.Free;
  end;
end;

{*************************************************}
procedure TALCardinalKeySessionAVLBinaryTree.Clear;
begin
  FcriticalSection.Acquire;
  try

    Inherited clear;

  finally
    FcriticalSection.Release;
  end;
end;

{***************************************************************************************}
function TALCardinalKeySessionAVLBinaryTree.Head: TALCardinalKeySessionAVLBinaryTreeNode;
begin
  FcriticalSection.Acquire;
  try

    Result := TALCardinalKeySessionAVLBinaryTreeNode(inherited head);

  finally
    FcriticalSection.Release;
  end;
end;

{****************************************************************************************}
function TALCardinalKeySessionAVLBinaryTree.First: TALCardinalKeySessionAVLBinaryTreeNode;
begin
  FcriticalSection.Acquire;
  try

    Result := TALCardinalKeySessionAVLBinaryTreeNode(inherited First);

  finally
    FcriticalSection.Release;
  end;
end;

{***************************************************************************************}
function TALCardinalKeySessionAVLBinaryTree.Last: TALCardinalKeySessionAVLBinaryTreeNode;
begin
  FcriticalSection.Acquire;
  try

    Result := TALCardinalKeySessionAVLBinaryTreeNode(inherited Last);

  finally
    FcriticalSection.Release;
  end;
end;

{**************************************************************************************************************************************}
function TALCardinalKeySessionAVLBinaryTree.Next(aNode: TALCardinalKeySessionAVLBinaryTreeNode): TALCardinalKeySessionAVLBinaryTreeNode;
begin
  FcriticalSection.Acquire;
  try

    Result := TALCardinalKeySessionAVLBinaryTreeNode(inherited Next(aNode));

  finally
    FcriticalSection.Release;
  end;
end;

{**************************************************************************************************************************************}
function TALCardinalKeySessionAVLBinaryTree.Prev(aNode: TALCardinalKeySessionAVLBinaryTreeNode): TALCardinalKeySessionAVLBinaryTreeNode;
begin
  FcriticalSection.Acquire;
  try

    Result := TALCardinalKeySessionAVLBinaryTreeNode(inherited Prev(aNode));

  finally
    FcriticalSection.Release;
  end;
end;

{***********************************************************************************************************************}
procedure TALCardinalKeySessionAVLBinaryTree.Iterate(Action: TALAVLBinaryTreeIterateFunc; Up: Boolean; ExtData: Pointer);
begin
  FcriticalSection.Acquire;
  try

    inherited Iterate(Action, Up, ExtData);

  finally
    FcriticalSection.Release;
  end;
end;

{**************************************************************************}
procedure TALCardinalKeySessionAVLBinaryTree.SaveToStream(Astream: Tstream);
Begin
  FcriticalSection.Acquire;
  try

    inherited SaveToStream(AStream);

  finally
    FcriticalSection.Release;
  end;
end;

{****************************************************************************}
procedure TALCardinalKeySessionAVLBinaryTree.LoadFromStream(Astream: Tstream);
Begin
  FcriticalSection.Acquire;
  try

    inherited LoadFromStream(AStream);

  finally
    FcriticalSection.Release;
  end;
end;

{****************************************************}
Function TALCardinalKeySessionAVLBinaryTree.NodeCount;
Begin
  FcriticalSection.Acquire;
  try

    Result := inherited NodeCount;

  finally
    FcriticalSection.Release;
  end;
end;

{**********************************************************************************************************************}
function TALCardinalKeySessionAVLBinaryTree.FindAndAcquireNode(idVal: Cardinal): TALCardinalKeySessionAVLBinaryTreeNode;
Var SuccessFullyAcquired: Boolean;
begin
  SuccessFullyAcquired := False;
  FcriticalSection.Acquire;
  try

    if (Now - FLastCheck > FCheckInterval) then begin
      DeleteExpiredNode;
      FLastCheck := Now;
    end;

    result := TALCardinalKeySessionAVLBinaryTreeNode(inherited FindNode(idVal));
    If result <> nil then begin
      result.LastAccess := now;
      If not Result.Acquired then begin
        SuccessFullyAcquired := True;
        Result.Acquired := True;
      end;
    end;

  finally
    FcriticalSection.Release;
  end;

  if (result <> nil) and (not SuccessFullyAcquired) then AcquireNode(Result);
end;

{******************************************************************************************************}
procedure TALCardinalKeySessionAVLBinaryTree.AcquireNode(aNode: TALCardinalKeySessionAVLBinaryTreeNode);

  {-----------------------------------------------------------------------------------}
  Function InternalAcquireNode(aNode: TALCardinalKeySessionAVLBinaryTreeNode): Boolean;
  Begin
    FcriticalSection.Acquire;
    try

      Result := not aNode.Acquired;
      If Result then aNode.Acquired := True;
      aNode.LastAccess := now;

    finally
      FcriticalSection.Release;
    end;
  end;

Begin

  While true do
   if InternalAcquireNode(aNode) then break;

end;

{**********************************************************************************************************************}
function TALCardinalKeySessionAVLBinaryTree.FindAndReleaseNode(idVal: Cardinal): TALCardinalKeySessionAVLBinaryTreeNode;
begin
  FcriticalSection.Acquire;
  try

    if (Now - FLastCheck > FCheckInterval) then begin
      DeleteExpiredNode;
      FLastCheck := Now;
    end;

    result := TALCardinalKeySessionAVLBinaryTreeNode(inherited FindNode(idVal));
    If result <> nil then begin
      result.LastAccess := now;
      Result.Acquired  := False;
    end;

  finally
    FcriticalSection.Release;
  end;
end;

{******************************************************************************************************}
procedure TALCardinalKeySessionAVLBinaryTree.ReleaseNode(aNode: TALCardinalKeySessionAVLBinaryTreeNode);
Begin
  FcriticalSection.Acquire;
  try

    aNode.Acquired := False;
    aNode.LastAccess := now;

  finally
    FcriticalSection.Release;
  end;
end;

{**************************************************************************************************************}
function TALCardinalKeySessionAVLBinaryTree.CreateAndAcquireSessionNode: TALCardinalKeySessionAVLBinaryTreeNode;
begin
  Result := CreateSessionNode;
  result.Acquired := True;
end;

end.
