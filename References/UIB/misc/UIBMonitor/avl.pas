(* The contents of this file are subject to the Mozilla Public License
   Version 1.1 (the "License"); you may not use this file except in compliance
   with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
   Software distributed under the License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.
*)

unit avl;

interface
{$ALIGN ON}
{$MINENUMSIZE 4}

const
  MAX_STACK = 32;

type

  TBranchDir = (bLeft, bRight);

  PIndexSlot = ^TIndexSlot;
  TIndexSlot = record
    FData: Pointer;
    FPrev: PIndexSlot;
    FNext: PIndexSlot;
  end;

  PIndexNode = ^TIndexNode;
  TIndexNode = record
    FBranch: array[TBranchDir] of PIndexNode;
    FFirst: TIndexSlot;
    FLast: PIndexSlot;
    FBalance: Integer;
  end;

  TStackNodeArray = array[0..MAX_STACK - 1] of PIndexNode;

  TIndexedList = class
  private
    FRoot: PIndexNode;
    FCount: Cardinal;
    FGeneration: Cardinal;
    FCursor: PIndexNode;
    FCursorSlot: PIndexSlot;
    FStack: TStackNodeArray;
    FStackSize: Cardinal;
    FNeedResync: boolean;
    procedure Resync;
    function GetData: Pointer;
    procedure DeleteItem(const p: PIndexNode);
  protected
    function doCompare(const item1, item2: Pointer): Integer; virtual;
    procedure doDelete(const item: Pointer); virtual;
  public
    constructor Create(unique: boolean); virtual;
    destructor Destroy; override;
    procedure Clear;
    function Add(const item: Pointer; unique: boolean): Pointer;
    function Delete(const item: Pointer): Pointer;
    function Find(const item: Pointer): Pointer;
    function First: Pointer;
    function Last: Pointer;
    function Prior: Pointer;
    function Next: Pointer;

    function FindNext: Pointer;
    function FindPrior: Pointer;
    property Data: Pointer read GetData;
    property Count: Cardinal read FCount;
  end;

implementation
uses SysUtils;

type
  TStackDirArray = array[0..MAX_STACK - 1] of TBranchDir;

{ TIndexdList }

function TIndexedList.doCompare(const item1, item2: Pointer): Integer;
begin
  result := 0;
end;

procedure TIndexedList.doDelete(const item: Pointer);
begin

end;

procedure TIndexedList.Resync;
var
  node, i: PIndexNode;
begin
  FNeedResync := False;
  if (FCursor <> nil) then
  begin
    node := FCursor;
    FStackSize := 0;
    i := FRoot;
    while i <> node do
    begin
      assert(FStackSize < MAX_STACK);
      assert(i <> nil);
      FStack[FStackSize] := i;
      inc(FStackSize);
      i := i.FBranch[TBranchDir(doCompare(node.FFirst.FData, i.FFirst.FData) > 0)];
    end;
  end;
end;

function TIndexedList.GetData: Pointer;
begin
  if FCursorSlot <> nil then
    Result := FCursorSlot.FData
  else
    Result := nil;
end;

constructor TIndexedList.Create(unique: boolean);
begin
  FRoot := nil;
  FCursor := nil;
  FCursorSlot := nil;
  FCount := 0;
  FGeneration := 0;
  FStackSize := 0;
  FNeedResync := False;
end;

function TIndexedList.Delete(const item: Pointer): Pointer;
var
  na: TStackNodeArray;
  da: TStackDirArray;
  k, j, comp: Integer;
  i: Pointer;
  dir: TBranchDir;
  p1, p2, p3, p4, p5, p6: PIndexNode;
begin
  assert(item <> nil);
  p3 := nil;
  k := 0;
  p1 := @FRoot;
  comp := -1;
  while comp <> 0 do
  begin
    dir := TBranchDir(comp > 0);
    na[k] := p1;
    da[k] := dir;
    inc(k);
    p1 := p1.FBranch[dir];
    if (p1 = nil) then
    begin
      Result := nil;
      exit;
    end;
    comp := doCompare(item, p1.FFirst.FData);
  end;
  i := p1.FFirst.FData;
  if (p1.FBranch[bRight] = nil) then
    na[k - 1].FBranch[da[k - 1]] := p1.FBranch[bLeft]
  else
  begin
    p2 := p1.FBranch[bRight];
    if (p2.FBranch[bLeft] = nil) then
    begin
      p2.FBranch[bLeft] := p1.FBranch[bLeft];
      p2.FBalance := p1.FBalance;
      na[k - 1].FBranch[da[k - 1]] := p2;
      da[k] := bRight;
      na[k] := p2;
      inc(k);
    end
    else
    begin
      j := k;
      inc(k);
      while True do
      begin
        da[k] := bLeft;
        na[k] := p2;
        inc(k);
        p3 := p2.FBranch[bLeft];
        if (p3.FBranch[bLeft] = nil) then
          break;
        p2 := p3;
      end;
      p3.FBranch[bLeft] := p1.FBranch[bLeft];
      p2.FBranch[bLeft] := p3.FBranch[bRight];
      p3.FBranch[bRight] := p1.FBranch[bRight];
      p3.FBalance := p1.FBalance;
      na[j - 1].FBranch[da[j - 1]] := p3;
      da[j] := bRight;
      na[j] := p3;
    end;
  end;
  DeleteItem(p1);
  assert(k > 0);
  dec(k);
  while (k > 0) do
  begin
    p4 := na[k];
    if (da[k] = bLeft) then
    begin
      inc(p4.FBalance);
      if (p4.FBalance = 1) then
        break
      else
        if (p4.FBalance = 2) then
        begin
          p5 := p4.FBranch[bRight];
          if (p5.FBalance = -1) then
          begin
            assert(p5.FBalance = -1);
            p6 := p5.FBranch[bLeft];
            p5.FBranch[bLeft] := p6.FBranch[bRight];
            p6.FBranch[bRight] := p5;
            p4.FBranch[bRight] := p6.FBranch[bLeft];
            p6.FBranch[bLeft] := p4;
            if (p6.FBalance = 1) then
            begin
              p5.FBalance := 0;
              p4.FBalance := -1;
            end
            else
              if (p6.FBalance = 0) then
              begin
                p5.FBalance := 0;
                p4.FBalance := 0;
              end
              else
              begin
                p5.FBalance := 1;
                p4.FBalance := 0;
              end;
            p6.FBalance := 0;
            na[k - 1].FBranch[da[k - 1]] := p6;
          end
          else
          begin
            p4.FBranch[bRight] := p5.FBranch[bLeft];
            p5.FBranch[bLeft] := p4;
            na[k - 1].FBranch[da[k - 1]] := p5;
            if (p5.FBalance = 0) then
            begin
              p5.FBalance := -1;
              p4.FBalance := 1;
              break;
            end
            else
            begin
              p5.FBalance := 0;
              p4.FBalance := 0;
            end;
          end;
        end;
    end
    else
    begin
      dec(p4.FBalance);
      if (p4.FBalance = -1) then
        break
      else
        if (p4.FBalance = -2) then
        begin
          p5 := p4.FBranch[bLeft];
          if (p5.FBalance = 1) then
          begin
            assert(p5.FBalance = 1);
            p6 := p5.FBranch[bRight];
            p5.FBranch[bRight] := p6.FBranch[bLeft];
            p6.FBranch[bLeft] := p5;
            p4.FBranch[bLeft] := p6.FBranch[bRight];
            p6.FBranch[bRight] := p4;
            if (p6.FBalance = -1) then
            begin
              p5.FBalance := 0;
              p4.FBalance := 1;
            end
            else
              if (p6.FBalance = 0) then
              begin
                p5.FBalance := 0;
                p4.FBalance := 0;
              end
              else
              begin
                p5.FBalance := -1;
                p4.FBalance := 0;
              end;
            p6.FBalance := 0;
            na[k - 1].FBranch[da[k - 1]] := p6;
          end
          else
          begin
            p4.FBranch[bLeft] := p5.FBranch[bRight];
            p5.FBranch[bRight] := p4;
            na[k - 1].FBranch[da[k - 1]] := p5;
            if (p5.FBalance = 0) then
            begin
              p5.FBalance := 1;
              p4.FBalance := -1;
              break;
            end
            else
            begin
              p5.FBalance := 0;
              p4.FBalance := 0;
            end;
          end;
        end;
    end;
    dec(k);
  end;
  dec(FCount);
  inc(FGeneration);
  Result := i;
end;

destructor TIndexedList.Destroy;
begin
  Clear;
  inherited;
end;

function TIndexedList.First: Pointer;
var
  p: PIndexNode;
begin
  FStackSize := 0;
  FNeedResync := False;
  p := FRoot;
  if (p <> nil) then
    while (p.FBranch[bLeft] <> nil) do
    begin
      assert(FStackSize < MAX_STACK);
      FStack[FStackSize] := p;
      inc(FStackSize);
      p := p.FBranch[bLeft];
    end;
  FCursor := p;
  if p <> nil then
  begin
    FCursorSlot := @p.FFirst;
    Result := FCursorSlot.FData
  end
  else
  begin
    FCursorSlot := nil;
    Result := nil;
  end;
end;

function TIndexedList.Last: Pointer;
var
  p: PIndexNode;
begin
  FStackSize := 0;
  FNeedResync := False;
  p := FRoot;
  if (p <> nil) then
    while (p.FBranch[bRight] <> nil) do
    begin
      assert(FStackSize < MAX_STACK);
      FStack[FStackSize] := p;
      inc(FStackSize);
      p := p.FBranch[bRight];
    end;
  FCursor := p;

  if p <> nil then
  begin
    FCursorSlot := FCursor.FLast;
    Result := FCursorSlot.FData
  end
  else
  begin
    FCursorSlot := nil;
    Result := nil;
  end;
end;

function TIndexedList.Find(const item: Pointer): Pointer;
var
  p, q: PIndexNode;
  comp: integer;
begin
  assert(item <> nil);
  FStackSize := 0;
  FNeedResync := False;
  p := FRoot;
  q := nil;
  while p <> nil do
  begin
    comp := doCompare(item, p.FFirst.FData);
    if (comp < 0) then
      q := p.FBranch[bLeft]
    else
      if (comp > 0) then
        q := p.FBranch[bRight]
      else
      begin
        FCursor := p;
        FCursorSlot := @p.FFirst;
        Result := FCursorSlot.FData;
        exit;
      end;
    assert(FStackSize < MAX_STACK);
    FStack[FStackSize] := p;
    inc(FStackSize);
    p := q;
  end;
  FStackSize := 0;
  FCursor := nil;
  FCursorSlot := nil;
  Result := nil;
end;

function TIndexedList.Add(const item: Pointer; unique: boolean): Pointer;
var
  da: TStackDirArray;
  p1, p2, p3, p4, p5, p6, p7: PIndexNode;
  i, comp: Integer;
  dir: TBranchDir;
begin
  assert(item <> nil);
  i := 0;
  p7 := @FRoot;
  p6 := FRoot;
  dir := bLeft;
  p3 := p7;
  p2 := p6;
  while p2 <> nil do
  begin
    comp := doCompare(item, p2.FFirst.FData);
    if (comp = 0) then
    begin
      if unique then
      begin
        FCursor := p2;
        FCursorSlot := @FCursor.FFirst;
        result := FCursorSlot.FData;
        doDelete(item);
      end else
      begin
        FCursor := p2;
        GetMem(FCursorSlot, SizeOf(TIndexSlot));
        FCursorSlot.FData := item;
        FCursorSlot.FPrev := FCursor.FLast;
        FCursorSlot.FNext := nil;
        FCursor.FLast.FNext := FCursorSlot;
        FCursor.FLast := FCursorSlot;
        FNeedResync := True;
        Result := FCursorSlot.FData;
        inc(FCount);
      end;
      exit;
    end;
    if (p2.FBalance <> 0) then
    begin
      p7 := p3;
      p6 := p2;
      i := 0;
    end;
    dir := TBranchDir(comp > 0);
    da[i] := dir;
    inc(i);
    p3 := p2;
    p2 := p2.FBranch[dir]
  end;

  GetMem(p1, sizeof(TIndexNode));
  p3.FBranch[dir] := p1;
  inc(FCount);
  p1.FFirst.FData := item;
  p1.FFirst.FNext := nil;
  p1.FFirst.FPrev := nil;
  p1.FLast := @p1.FFirst;
  p1.FBranch[bLeft] := nil;
  p1.FBranch[bRight] := nil;
  p1.FBalance := 0;
  if (p6 = nil) then
  begin
    FCursor := p1;
    FCursorSlot := @p1.FFirst;
    FNeedResync := True;
    Result := FCursorSlot.FData;
    exit;
  end;

  p2 := p6;
  i := 0;
  while p2 <> p1 do
  begin
    if (da[i] = bLeft) then
      dec(p2.FBalance)
    else
      inc(p2.FBalance);
    p2 := p2.FBranch[da[i]];
    inc(i);
  end;

  if (p6.FBalance = -2) then
  begin
    p5 := p6.FBranch[bLeft];
    if (p5.FBalance = -1) then
    begin
      p4 := p5;
      p6.FBranch[bLeft] := p5.FBranch[bRight];
      p5.FBranch[bRight] := p6;
      p5.FBalance := 0;
      p6.FBalance := 0;
    end
    else
    begin
      assert(p5.FBalance = 1);
      p4 := p5.FBranch[bRight];
      p5.FBranch[bRight] := p4.FBranch[bLeft];
      p4.FBranch[bLeft] := p5;
      p6.FBranch[bLeft] := p4.FBranch[bRight];
      p4.FBranch[bRight] := p6;
      if (p4.FBalance = -1) then
      begin
        p5.FBalance := 0;
        p6.FBalance := 1;
      end
      else
        if (p4.FBalance = 0) then
        begin
          p5.FBalance := 0;
          p6.FBalance := 0;
        end
        else
        begin
          p5.FBalance := -1;
          p6.FBalance := 0;
        end;
      p4.FBalance := 0;
    end;
  end
  else
    if (p6.FBalance = 2) then
    begin
      p5 := p6.FBranch[bRight];
      if (p5.FBalance = 1) then
      begin
        p4 := p5;
        p6.FBranch[bRight] := p5.FBranch[bLeft];
        p5.FBranch[bLeft] := p6;
        p5.FBalance := 0;
        p6.FBalance := 0;
      end
      else
      begin
        assert(p5.FBalance = -1);
        p4 := p5.FBranch[bLeft];
        p5.FBranch[bLeft] := p4.FBranch[bRight];
        p4.FBranch[bRight] := p5;
        p6.FBranch[bRight] := p4.FBranch[bLeft];
        p4.FBranch[bLeft] := p6;
        if (p4.FBalance = 1) then
        begin
          p5.FBalance := 0;
          p6.FBalance := -1;
        end
        else
          if (p4.FBalance = 0) then
          begin
            p5.FBalance := 0;
            p6.FBalance := 0;
          end
          else
          begin
            p5.FBalance := 1;
            p6.FBalance := 0;
          end;
        p4.FBalance := 0;
      end;
    end
    else
    begin
      FCursor := p1;
      FCursorSlot := @p1.FFirst;
      FNeedResync := True;
      Result := FCursorSlot.FData;
      exit;
    end;
  p7.FBranch[TBranchDir(p6 <> p7.FBranch[bLeft])] := p4;
  inc(FGeneration);
  FCursor := p1;
  FCursorSlot := @p1.FFirst;
  FNeedResync := True;
  Result := FCursorSlot.FData;
end;

function TIndexedList.Next: Pointer;
var
  p1, p2: PIndexNode;
begin
  if FNeedResync then
    Resync;

  Result := FindNext;
  if Result <> nil then
    Exit;

  p1 := FCursor;
  if (p1 = nil) then
  begin
    Result := First;
    exit;
  end
  else
    if (p1.FBranch[bRight] <> nil) then
    begin
      assert(FStackSize < MAX_STACK);
      FStack[FStackSize] := p1;
      inc(FStackSize);
      p1 := p1.FBranch[bRight];
      while (p1.FBranch[bLeft] <> nil) do
      begin
        assert(FStackSize < MAX_STACK);
        FStack[FStackSize] := p1;
        inc(FStackSize);
        p1 := p1.FBranch[bLeft];
      end;
    end
    else
      repeat
        if (FStackSize = 0) then
        begin
          FCursor := nil;
          FCursorSlot := nil;
          Result := nil;
          exit;
        end;
        p2 := p1;
        dec(FStackSize);
        p1 := FStack[FStackSize];
      until (p2 <> p1.FBranch[bRight]);
  FCursor := p1;
  FCursorSlot := @p1.FFirst;
  Result := FCursorSlot.FData;
end;

function TIndexedList.Prior: Pointer;
var
  p1, p2: PIndexNode;
begin
  if FNeedResync then
    Resync;

  Result := FindPrior;
  if Result <> nil then
    Exit;

  p1 := FCursor;
  if (p1 = nil) then
  begin
    Result := Last;
    exit;
  end
  else
    if (p1.FBranch[bLeft] <> nil) then
    begin
      assert(FStackSize < MAX_STACK);
      FStack[FStackSize] := p1;
      inc(FStackSize);
      p1 := p1.FBranch[bLeft];
      while (p1.FBranch[bRight] <> nil) do
      begin
        assert(FStackSize < MAX_STACK);
        FStack[FStackSize] := p1;
        inc(FStackSize);
        p1 := p1.FBranch[bRight];
      end;
    end
    else
      repeat
        if (FStackSize = 0) then
        begin
          FCursor := nil;
          FCursorSlot := nil;
          Result := nil;
          exit;
        end;
        p2 := p1;
        dec(FStackSize);
        p1 := FStack[FStackSize];
      until (p2 <> p1.FBranch[bLeft]);
  FCursor := p1;
  FCursorSlot := @p1.FFirst;
  Result := FCursorSlot.FData;
end;

procedure TIndexedList.Clear;
var
  p1, p2: PIndexNode;
begin
  p1 := FRoot;
  while p1 <> nil do
  begin
    if (p1.FBranch[bLeft] = nil) then
    begin
      p2 := p1.FBranch[bRight];
      DeleteItem(p1);
    end
    else
    begin
      p2 := p1.FBranch[bLeft];
      p1.FBranch[bLeft] := p2.FBranch[bRight];
      p2.FBranch[bRight] := p1;
    end;
    p1 := p2;
  end;
  FRoot := nil;
  FCount := 0;
  FGeneration := 0;
  FCursor := nil;
  FCursorSlot := nil;
  FStackSize := 0;
  FNeedResync := False;
end;

procedure TIndexedList.DeleteItem(const p: PIndexNode);
var
  p1, p2: PIndexSlot;
begin
  doDelete(p.FFirst.FData);
  p1 := p.FFirst.FNext;
  while p1 <> nil do
  begin
    if p1.FData <> nil then
      doDelete(p1.FData);
    p2 := p1;
    p1 := p2.FNext;
    FreeMem(p2);
    dec(FCount);
  end;
  FreeMem(p);
end;

function TIndexedList.FindNext: Pointer;
begin
  if FCursorSlot <> nil then
  begin
    FCursorSlot := FCursorSlot.FNext;
    if FCursorSlot <> nil then
      Result := FCursorSlot.FData else
      Result := nil;
  end else
    Result := nil
end;

function TIndexedList.FindPrior: Pointer;
begin
  if FCursorSlot <> nil then
  begin
    FCursorSlot := FCursorSlot.FPrev;
    if FCursorSlot <> nil then
      Result := FCursorSlot.FData else
      Result := nil;
  end else
    Result := nil
end;

end.

