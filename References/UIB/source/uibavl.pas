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
(* Version: 1.5  Author: Walt Karas                                             *)
(* http://www.geocities.com/wkaras/gen_c/cavl_tree.html                         *)
(*                                                                              *)
(********************************************************************************)

unit uibavl;
{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
interface

const
  AVL_MAX_DEPTH = sizeof(longint) * 8;
  AVL_NULL = nil;

type
  TAvlSize = longword;
  TAvlKey = Pointer;
  TAvlBuildIterType = Pointer;
  TAvlBitArray = set of 0..AVL_MAX_DEPTH - 1;

  TAvlSearchType = (stEQual, stLess, stGreater);
  TAvlSearchTypes = set of TAvlSearchType;

  TAvlHandle = class
  private
    FGt, FLt: TAvlHandle;
    FBf: integer;
  end;

  TAvlTree = class
  private
    FRoot: TAvlHandle;
    function balance(bal: TAvlHandle): TAvlHandle;
  protected
    function CompareNodeNode(node1, node2: TAvlHandle): integer; virtual;
    function CompareKeyNode(k: TAvlKey; h: TAvlHandle): integer; virtual;
    function BuildIterVal(p: TAvlBuildIterType): TAvlHandle; virtual;
    procedure BuildIterIncr(p: TAvlBuildIterType); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IsEmpty: boolean;
    function Insert(h: TAvlHandle): TAvlHandle;
    function Search(k: TAvlKey; st: TAvlSearchTypes = [stEqual]): TAvlHandle;
    function First: TAvlHandle;
    function Last: TAvlHandle;
    function Remove(k: TAvlKey): TAvlHandle;
    function Subst(newnode: TAvlHandle): TAvlHandle;
    function Build(p: TAvlBuildIterType; NumNodes: TAvlSize): integer;
    procedure Clear; virtual;
  end;

  TAvlIterator = class
  private
    { Tree being iterated over. }
    FTree: TAvlTree;
    { Records a path into the tree.  If bit n is true, indicates
      take greater branch from the nth node in the path, otherwise
      take the less branch.  bit 0 gives branch from root, and
      so on. }
    FBranch: TAvlBitArray;
    { Zero-based depth of path into tree. }
    FDepth: longint; //unsigned;
    { Handles of nodes in path from root to current node (returned by * ). }
    FPath: array[0..AVL_MAX_DEPTH - 2] of TAvlHandle;
  public
    { Iterator function prototypes. }
    constructor Create(tree: TAvlTree); virtual;
    procedure Search(k: TAvlKey; st: TAvlSearchTypes = [stEQual]);
    procedure First;
    procedure Last;
    function GetIter: TAvlHandle;
    procedure Next;
    procedure Prior;
  end;

implementation
uses SysUtils;

const
  MASK_HIGH_BIT = not ((not longword(0)) shr 1);

(* The presumed reason that an instantiation places additional fields
** inside the AVL tree structure is that the SET_ and GET_ macros
** need these fields.  The "balance" function does not explicitly use
** any fields in the AVL tree structure, so only pass an AVL tree
** structure pointer to "balance" if it has instantiation-specific
** fields that are (presumably) needed by the SET_/GET_ calls within
** "balance".
*)

constructor TAvlTree.Create;
begin
  FRoot := AVL_NULL;
end;

destructor TAvlTree.Destroy;
begin
  Clear;
  inherited;
end;

function TAvlTree.IsEmpty: boolean;
begin
  result := FRoot = AVL_NULL;
end;

(* Put the private balance function in the same compilation module as
** the insert function.  *)

(* Balances subtree, returns handle of root node of subtree after balancing.
*)
function TAvlTree.balance(bal: TAvlHandle): TAvlHandle;
var
  deep, old: TAvlHandle;
  bf: integer;
begin
  (* Either the "greater than" or the "less than" subtree of
  ** this node has to be 2 levels deeper (or else it wouldn't
  ** need balancing).
  *)
  if (bal.FBf > 0) then
  begin
    (* "Greater than" subtree is deeper. *)
    deep := bal.FGt;
    if (deep.FBf < 0) then
    begin
      old := bal;
      bal := deep.FLt;
      old.FGt := bal.FLt;
      deep.FLt := bal.FGt;
      bal.FLt := old;
      bal.FGt := deep;
      bf := bal.FBf;
      if (bf <> 0) then
      begin
        if (bf > 0) then
        begin
          old.FBf := -1;
          deep.FBf := 0;
        end else
        begin
          deep.FBf := 1;
          old.FBf := 0;
        end;
        bal.FBf := 0;
      end else
      begin
        old.FBf := 0;
        deep.FBf := 0;
      end;
    end else
    begin
      bal.FGt := deep.FLt;
      deep.FLt := bal;
      if (deep.FBf = 0) then
      begin
        deep.FBf := -1;
        bal.FBf := 1;
      end else
      begin
        deep.FBf := 0;
        bal.FBf := 0;
      end;
      bal := deep;
    end;
  end else
  begin
    (* "Less than" subtree is deeper. *)

    deep := bal.FLt;
    if (deep.FBf > 0) then
    begin
      old := bal;
      bal := deep.FGt;
      old.FLt := bal.FGt;
      deep.FGt := bal.FLt;
      bal.FGt := old;
      bal.FLt := deep;

      bf := bal.FBf;
      if (bf <> 0) then
      begin
        if (bf < 0) then
        begin
          old.FBf := 1;
          deep.FBf := 0;
        end else
        begin
          deep.FBf := -1;
          old.FBf := 0;
        end;
        bal.FBf := 0;
      end else
      begin
        old.FBf := 0;
        deep.FBf := 0;
      end;
    end else
    begin
      bal.FLt := deep.FGt;
      deep.FGt := bal;
      if (deep.FBf = 0) then
      begin
        deep.FBf := 1;
        bal.FBf := -1;
      end else
      begin
        deep.FBf := 0;
        bal.FBf := 0;
      end;
      bal := deep;
    end;
  end;
  Result := bal;
end;

function TAvlTree.Insert(h: TAvlHandle): TAvlHandle;
var
  unbal, parentunbal, hh, parent: TAvlHandle;
  depth, unbaldepth: longint;
  cmp: integer;
  (* Balance factor of last unbalanced node. *)
  unbalbf: integer;
  (* Records a path into the tree.  If bit n is true, indicates
  ** take greater branch from the nth node in the path, otherwise
  ** take the less branch.  bit 0 gives branch from root, and
  ** so on. *)
  branch: TAvlBitArray;
begin
  h.FLt := AVL_NULL;
  h.FGt := AVL_NULL;
  h.FBf := 0;
  branch := [];

  if (FRoot = AVL_NULL) then
    FRoot := h
  else
  begin
    (* Last unbalanced node encountered in search for insertion point. *)
    unbal := AVL_NULL;
    (* Parent of last unbalanced node. *)
    parentunbal := AVL_NULL;

    (* Zero-based depth in tree. *)
    depth := 0;
    unbaldepth := 0;

    hh := FRoot;
    parent := AVL_NULL;
    repeat
      if (hh.FBf <> 0) then
      begin
        unbal := hh;
        parentunbal := parent;
        unbaldepth := depth;
      end;
      cmp := CompareNodeNode(h, hh);
      if (cmp = 0) then
      begin
        (* Duplicate key. *)
        Result := hh;
        h.Free;
        exit;
      end;
      parent := hh;
      if (cmp > 0) then
      begin
        hh := hh.FGt;
        include(branch, depth);
      end else
      begin
        hh := hh.FLt;
        exclude(branch, depth);
      end;
      inc(depth);
    until (hh = AVL_NULL);

    (*  Add node to insert as leaf of tree. *)
    if (cmp < 0) then
      parent.FLt := h else
      parent.FGt := h;

    depth := unbaldepth;

    if (unbal = AVL_NULL) then
      hh := FRoot
    else
    begin
      if depth in branch then
        cmp := 1 else
        cmp := -1;
      inc(depth);
      unbalbf := unbal.FBf;
      if (cmp < 0) then
        dec(unbalbf) else (* cmp > 0 *)
        inc(unbalbf);
      if cmp < 0 then
        hh := unbal.FLt else
        hh := unbal.FGt;
      if ((unbalbf <> -2) and (unbalbf <> 2)) then
      begin
        (* No rebalancing of tree is necessary. *)
        unbal.FBf := unbalbf;
        unbal := AVL_NULL;
      end;
    end;

    if (hh <> AVL_NULL) then
      while (h <> hh) do
      begin
        if depth in branch then
          cmp := 1 else
          cmp := -1;
        inc(depth);
        if (cmp < 0) then
        begin
          hh.FBf := -1;
          hh := hh.FLt;
        end else (* cmp > 0 *)
        begin
          hh.FBf := 1;
          hh := hh.FGt;
        end;
      end;

    if (unbal <> AVL_NULL) then
    begin
      unbal := balance(unbal);
      if (parentunbal = AVL_NULL) then
        FRoot := unbal
      else
      begin
        depth := unbaldepth - 1;
        if depth in branch then
          cmp := 1 else
          cmp := -1;
        if (cmp < 0) then
          parentunbal.FLt := unbal else (* cmp > 0 *)
          parentunbal.FGt := unbal;
      end;
    end;
  end;
  result := h;
end;

function TAvlTree.Search(k: TAvlKey; st: TAvlSearchTypes): TAvlHandle;
var
  cmp, target_cmp: integer;
  match_h, h: TAvlHandle;
begin
  match_h := AVL_NULL;
  h := FRoot;

  if (stLess in st) then
    target_cmp := 1 else
    if (stGreater in st) then
      target_cmp := -1 else
      target_cmp := 0;

  while (h <> AVL_NULL) do
  begin
    cmp := CompareKeyNode(k, h);
    if (cmp = 0) then
    begin
      if (stEqual in st) then
      begin
        match_h := h;
        break;
      end;
      cmp := -target_cmp;
    end
    else
    if (target_cmp <> 0) then
      if ((cmp xor target_cmp) and MASK_HIGH_BIT) = 0 then
        (* cmp and target_cmp are both positive or both negative. *)
        match_h := h;
    if cmp < 0 then
      h := h.FLt else
      h := h.FGt;
  end;
  result := match_h;
end;

function TAvlTree.First: TAvlHandle;
var
  h, parent: TAvlHandle;
begin
  h := FRoot;
  parent := AVL_NULL;
  while (h <> AVL_NULL) do
  begin
    parent := h;
    h := h.FLt;
  end;
  Result := parent;
end;

function TAvlTree.Last: TAvlHandle;
var
  h, parent: TAvlHandle;
begin
  h := FRoot;
  parent := AVL_NULL;
  while (h <> AVL_NULL) do
  begin
    parent := h;
    h := h.FGt;
  end;
  Result := parent;
end;

function TAvlTree.Remove(k: TAvlKey): TAvlHandle;
var
  depth, rm_depth: longint;
  (* Records a path into the tree.  If bit n is true, indicates
  ** take greater branch from the nth node in the path, otherwise
  ** take the less branch.  bit 0 gives branch from root, and
  ** so on. *)
  branch: TAvlBitArray;
  h, parent, child, path, rm, parent_rm: TAvlHandle;
  cmp, cmp_shortened_sub_with_path, reduced_depth, bf: integer;
begin
  cmp_shortened_sub_with_path := 0;
  branch := [];

  (* Zero-based depth in tree. *)
  depth := 0;
  h := FRoot;
  parent := AVL_NULL;
  while true do
  begin
    if (h = AVL_NULL) then
    begin
      (* No node in tree with given key. *)
      result := AVL_NULL;
      exit;
    end;
    cmp := CompareKeyNode(k, h);
    if (cmp = 0) then
      (* Found node to remove. *)
      break;
    parent := h;
    if (cmp > 0) then
    begin
      h := h.FGt;
      include(branch, depth)
    end else
    begin
      h := h.FLt;
      exclude(branch, depth)
    end;
    inc(depth);
    cmp_shortened_sub_with_path := cmp;
  end;
  rm := h;
  parent_rm := parent;
  rm_depth := depth;

  (* If the node to remove is not a leaf node, we need to get a
  ** leaf node, or a node with a single leaf as its child, to put
  ** in the place of the node to remove.  We will get the greatest
  ** node in the less subtree (of the node to remove), or the least
  ** node in the greater subtree.  We take the leaf node from the
  ** deeper subtree, if there is one. *)

  if (h.FBf < 0) then
  begin
    child := h.FLt;
    exclude(branch, depth);
    cmp := -1;
  end else
  begin
    child := h.FGt;
    include(branch, depth);
    cmp := 1;
  end;
  inc(depth);

  if (child <> AVL_NULL) then
  begin
    cmp := -cmp;
    repeat
      parent := h;
      h := child;
      if (cmp < 0) then
      begin
        child := h.FLt;
        exclude(branch, depth);
      end else
      begin
        child := h.FGt;
        include(branch, depth);
      end;
      inc(depth);
    until (child = AVL_NULL);

    if (parent = rm) then
      (* Only went through do loop once.  Deleted node will be replaced
      ** in the tree structure by one of its immediate children. *)
      cmp_shortened_sub_with_path := -cmp else
      cmp_shortened_sub_with_path := cmp;

    (* Get the handle of the opposite child, which may not be null. *)
    if cmp > 0 then
      child := h.FLt else
      child := h.FGt;
  end;

  if (parent = AVL_NULL) then
    (* There were only 1 or 2 nodes in this tree. *)
    FRoot := child else
    if (cmp_shortened_sub_with_path < 0) then
      parent.FLt := child else
      parent.FGt := child;

  (* "path" is the parent of the subtree being eliminated or reduced
  ** from a depth of 2 to 1.  If "path" is the node to be removed, we
  ** set path to the node we're about to poke into the position of the
  ** node to be removed. *)
  if parent = rm then
    path := h else
    path := parent;

  if (h <> rm) then
  begin
    (* Poke in the replacement for the node to be removed. *)
    h.FLt := rm.FLt;
    h.FGt := rm.FGt;
    h.FBf := rm.FBf;
    if (parent_rm = AVL_NULL) then
      FRoot := h
    else
    begin
      depth := rm_depth - 1;
      if (depth in branch) then
        parent_rm.FGt := h else
        parent_rm.FLt := h;
    end;
  end;

  if (path <> AVL_NULL) then
  begin
    (* Create a temporary linked list from the parent of the path node
    ** to the root node. *)
    h := FRoot;
    parent := AVL_NULL;
    depth := 0;
    while (h <> path) do
    begin
      if (depth in branch) then
      begin
        child := h.FGt;
        h.FGt := parent;
      end else
      begin
        child := h.FLt;
        h.FLt := parent;
      end;
      inc(depth);
      parent := h;
      h := child;
    end;

    (* Climb from the path node to the root node using the linked
    ** list, restoring the tree structure and rebalancing as necessary.
    *)
    reduced_depth := 1;
    cmp := cmp_shortened_sub_with_path;
    while true do
    begin
      if (reduced_depth <> 0) then
      begin
        bf := h.FBf;
        if (cmp < 0) then
          inc(bf) else (* cmp > 0 *)
          dec(bf);
        if ((bf = -2) or (bf = 2)) then
        begin
          h := balance(h);
          bf := h.FBf;
        end else
          h.FBf := bf;
        reduced_depth := integer(bf = 0);
      end;
      if (parent = AVL_NULL) then
        break;
      child := h;
      h := parent;
      dec(depth);
      if depth in branch then
        cmp := 1 else
        cmp := -1;
      if (cmp < 0) then
      begin
        parent := h.FLt;
        h.FLt := child;
      end else
      begin
        parent := h.FGt;
        h.FGt := child;
      end;
    end;
    FRoot := h;
  end;
  result := rm;
end;

function TAvlTree.Subst(newnode: TAvlHandle): TAvlHandle;
var
  h, parent: TAvlHandle;
  cmp, last_cmp: integer;
begin
  h := FRoot;
  parent := AVL_NULL;
  last_cmp := 0;

  (* Search for node already in tree with same key. *)
  while true do
  begin
    if (h = AVL_NULL) then
    begin
      (* No node in tree with same key as new node. *)
      Result := AVL_NULL;
      exit;
    end;
    cmp := CompareNodeNode(newnode, h);
    if (cmp = 0) then
      (* Found the node to substitute new one for. *)
      break;
    last_cmp := cmp;
    parent := h;
    if cmp < 0 then
      h := h.FLt else
      h := h.FGt;
  end;

  (* Copy tree housekeeping fields from node in tree to new node. *)
  newnode.FLt := h.FLt;
  newnode.FGt := h.FGt;
  newnode.FBf := h.FBf;

  if (parent = AVL_NULL) then
    (* New node is also new root. *)
    FRoot := newnode
  else
  begin
    (* Make parent point to new node. *)
    if (last_cmp < 0) then
      parent.FLt := newnode else
      parent.FGt := newnode;
  end;
  Result := h;
end;

function TAvlTree.Build(p: TAvlBuildIterType; NumNodes: TAvlSize): integer;
var
  (* Gives path to subtree being built.  If bit n is false, branch
  ** less from the node at depth n, if true branch greater. *)
  branch: TAvlBitArray;
  (* If bit n is true, then for the current subtree at depth n, its
  ** greater subtree has one more node than its less subtree. *)
  rem: TAvlBitArray;
  depth: longint;
  numsub: TAvlSize;
  (* h is root of current subtree, child is one of its children. *)
  lessparent, h, child: TAvlHandle;
begin
  h := nil;
  (* Depth of root node of current subtree. *)
  depth := 0;
  (* Number of nodes in current subtree. *)
  numsub := NumNodes;

  rem := [];
  branch := [];

  (* The algorithm relies on a stack of nodes whose less subtree has
  ** been built, but whose greater subtree has not yet been built.
  ** The stack is implemented as linked list.  The nodes are linked
  ** together by having the "greater" handle of a node set to the
  ** next node in the list.  "less_parent" is the handle of the first
  ** node in the list. *)
  lessparent := AVL_NULL;

  if (NumNodes = 0) then
  begin
    FRoot := AVL_NULL;
    result := 1;
    exit;
  end;

  while true do
  begin
    while (numsub > 2) do
    begin
      (* Subtract one for root of subtree. *)
      dec(numsub);
      if (numsub and 1) <> 0 then
        include(rem, depth) else
        exclude(rem, depth);
      exclude(branch, depth);
      inc(depth);
      numsub := numsub shr 1;
    end;

    if (numsub = 2) then
    begin
      (* Build a subtree with two nodes, slanting to greater.
      ** I arbitrarily chose to always have the extra node in the
      ** greater subtree when there is an odd number of nodes to
      ** split between the two subtrees. *)

      h := BuildIterVal(p);
      BuildIterIncr(p);
      child := BuildIterVal(p);
      BuildIterIncr(p);
      child.FLt := AVL_NULL;
      child.FGt := AVL_NULL;
      child.FBf := 0;
      h.FGt := child;
      h.FLt := AVL_NULL;
      h.FBf := 1;
    end else (* numsub == 1 *)
    begin
      (* Build a subtree with one node. *)

      h := BuildIterVal(p);
      BuildIterIncr(p);
      h.FLt := AVL_NULL;
      h.FGt := AVL_NULL;
      h.FBf := 0;
    end;

    while (depth <> 0) do
    begin
      dec(depth);
      if (not (depth in branch)) then
        (* We've completed a less subtree. *)
        break;

      (* We've completed a greater subtree, so attach it to
      ** its parent (that is less than it).  We pop the parent
      ** off the stack of less parents. *)
      child := h;
      h := lessparent;
      lessparent := h.FGt;
      h.FGt := child;
      (* numsub := 2 * (numsub - rem[depth]) + rem[depth] + 1 *)
      numsub := numsub shl 1;
      if not (depth in rem) then
        inc(numsub);
      if (numsub and (numsub - 1)) <> 0 then
        (* numsub is not a power of 2. *)
        h.FBf := 0 else
        (* numsub is a power of 2. *)
        h.FBf := 1;
    end;

    if (numsub = NumNodes) then
      (* We've completed the full tree. *)
      break;

    (* The subtree we've completed is the less subtree of the
    ** next node in the sequence. *)

    child := h;
    h := BuildIterVal(p);
    BuildIterIncr(p);
    h.FLt := child;

    (* Put h into stack of less parents. *)
    h.FGt := lessparent;
    lessparent := h;

    (* Proceed to creating greater than subtree of h. *)
    include(branch, depth);
    if depth in rem then
      inc(numsub);
    inc(depth);
  end; (* end while *)
  FRoot := h;
  result := 1;
end;

procedure TAvlTree.Clear;
var
  node1, node2: TAvlHandle;
begin
  node1 := FRoot;
  while node1 <> nil do
  begin
    if (node1.FLt = nil) then
    begin
      node2 := node1.FGt;
      node1.Free;
    end
    else
    begin
      node2 := node1.FLt;
      node1.FLt := node2.FGt;
      node2.FGt := node1;
    end;
    node1 := node2;
  end;
  FRoot := nil;
end;

procedure TAvlTree.BuildIterIncr(p: TAvlBuildIterType);
begin
  raise Exception.Create('not implemented');
end;

function TAvlTree.BuildIterVal(p: TAvlBuildIterType): TAvlHandle;
begin
//  Result := nil;
  raise Exception.Create('not implemented');
end;

function TAvlTree.CompareKeyNode(k: TAvlKey; h: TAvlHandle): integer;
begin
//  Result := 0;
  raise Exception.Create('not implemented');
end;

function TAvlTree.CompareNodeNode(node1, node2: TAvlHandle): integer;
begin
//  Result := 0;
  raise Exception.Create('not implemented');
end;

{ TAvlIterator }

(* Initialize depth to invalid value, to indicate iterator is
** invalid.   (Depth is zero-base.)  It's not necessary to initialize
** iterators prior to passing them to the "start" function.
*)

constructor TAvlIterator.Create(tree: TAvlTree);
begin
  FDepth := not 0;
  FTree := tree;
end;

procedure TAvlIterator.Search(k: TAvlKey; st: TAvlSearchTypes);
var
  h: TAvlHandle;
  d: longint;
  cmp, target_cmp: integer;
begin
  h := FTree.FRoot;
  d := 0;

  (* Save the tree that we're going to iterate through in a
  ** member variable. *)
  FDepth := not 0;

  if (h = AVL_NULL) then
    (* Tree is empty. *)
    exit;

  if (stLess in st) then
    (* Key can be greater than key of starting node. *)
    target_cmp := 1 else
      if (stGreater in st) then
        (* Key can be less than key of starting node. *)
        target_cmp := -1 else
          (* Key must be same as key of starting node. *)
          target_cmp := 0;

  while true do
  begin
    cmp := FTree.CompareKeyNode(k, h);
    if (cmp = 0) then
    begin
      if (stEqual in st) then
      begin
        (* Equal node was sought and found as starting node. *)
        FDepth := d;
        break;
      end;
      cmp := -target_cmp;
    end
    else
    if (target_cmp <> 0) then
      if ((cmp xor target_cmp) and MASK_HIGH_BIT) = 0 then
        (* cmp and target_cmp are both negative or both positive. *)
        FDepth := d;
    if cmp < 0 then
      h := h.FLt else
      h := h.FGt;
    if (h = AVL_NULL) then
      break;
    if (cmp > 0) then
      include(FBranch, d) else
      exclude(FBranch, d);
    FPath[d] := h;
    inc(d);
  end;
end;

procedure TAvlIterator.First;
var
  h: TAvlHandle;
begin
  h := FTree.FRoot;
  FDepth := not 0;
  FBranch := [];

  while (h <> AVL_NULL) do
  begin
    if (FDepth <> not 0) then
      FPath[FDepth] := h;
    inc(FDepth);
    h := h.FLt;
  end;
end;

procedure TAvlIterator.Last;
var
  h: TAvlHandle;
begin
  h := FTree.FRoot;
  FDepth := not 0;
  FBranch := [0..AVL_MAX_DEPTH - 1];
  while (h <> AVL_NULL) do
  begin
    if (FDepth <> not 0) then
      FPath[FDepth] := h;
    inc(FDepth);
    h := h.FGt;
  end;
end;

function TAvlIterator.GetIter: TAvlHandle;
begin
  if (FDepth = not 0) then
  begin
    result := AVL_NULL;
    exit;
  end;

  if FDepth = 0 then
    Result := FTree.FRoot else
    Result := FPath[FDepth - 1];
end;

procedure TAvlIterator.Next;
var
  h: TAvlHandle;
begin
  if (FDepth <> not 0) then
  begin
    if FDepth = 0 then
      h := FTree.FRoot.FGt else
      h := FPath[FDepth - 1].FGt;

    if (h = AVL_NULL) then
      repeat
        if (FDepth = 0) then
        begin
          FDepth := not 0;
          break;
        end;
        dec(FDepth);
      until (not (FDepth in FBranch))
    else
    begin
      include(FBranch, FDepth);
      FPath[FDepth] := h;
      inc(FDepth);
      while true do
      begin
        h := h.FLt;
        if (h = AVL_NULL) then
          break;
        exclude(FBranch, FDepth);
        FPath[FDepth] := h;
        inc(FDepth);
      end;
    end;
  end;
end;

procedure TAvlIterator.Prior;
var
  h: TAvlHandle;
begin
  if (FDepth <> not 0) then
  begin
    if FDepth = 0 then
      h := FTree.FRoot.FLt else
      h := FPath[FDepth - 1].FLt;
    if (h = AVL_NULL) then
      repeat
        if (FDepth = 0) then
        begin
          FDepth := not 0;
          break;
        end;
        dec(FDepth);
      until (FDepth in FBranch)
    else
    begin
      exclude(FBranch, FDepth);
      FPath[FDepth] := h;
      inc(FDepth);
      while true do
      begin
        h := h.FGt;
        if (h = AVL_NULL) then
          break;
        include(FBranch, FDepth);
        FPath[FDepth] := h;
        inc(FDepth);
      end;
    end;
  end;
end;

end.
