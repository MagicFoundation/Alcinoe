{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          https://alcinoe.svn.sourceforge.net/svnroot/alcinoe              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALStringList
Version:      3.50

Description:  TALStringList in inherited from Delphi TstringList.
              It's allow to search a name=value using a quicksort
              algorithm when the list is sorted.
              TALAVLStringList it's a TStringlist Like, but use
              internaly an AVL binary Tree to handle big indexed
              list

Legal issues: Copyright (C) 1999-2010 by Arkadia Software Engineering

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

History :     27/10/2007 add ForceValues and ForceValueFromIndex
                         that do not delete an entry when we do
                         ForceValue[name] := ''
              26/01/2009 add TALAVLStringList

Link :

* Please send all your feedback to svanderclock@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  voting on http://www.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALStringList;

interface

Uses Windows,
     Classes,
     sysutils,
     Contnrs,
     AlAvlBinaryTRee;

Type

  {--------------------------------}
  TALStringList = class(TStringList)
  private
    function GetForceValue(const Name: string): string;
    function GetForceValueFromIndex(Index: Integer): string;
    procedure SetForceValue(const Name, Value: string);
    procedure SetForceValueFromIndex(Index: Integer; const Value: string);
  protected
    procedure Put(Index: Integer; const S: string); override;
    function CompareStrings(const S1, S2: string): Integer; override;
  public
    function FindName(const S: string; var Index: Integer): Boolean; virtual;
    function IndexOfName(const Name: string): Integer; override;
    property ForceValues[const Name: string]: string read GetForceValue write SetForceValue;
    property ForceValueFromIndex[Index: Integer]: string read GetForceValueFromIndex write SetForceValueFromIndex;
  end;

  {-----------------------}
  TALAVLStringList = class;
  TALAVLStringListSortCompare = function(List: TALAVLStringList; Index1, Index2: Integer): Integer;

  {-------------------------------------------------------------------}
  TALAVLStringListBinaryTreeNode = class(TALStringKeyAVLBinaryTreeNode)
  Private
  Protected
  Public
    Val: String;  // Value
    Obj: Tobject; // Object
    Idx: integer; // Index in the NodeList
    Nvs: Boolean; // if NameValueSeparator was present
    Constructor Create; Override;
  end;

  {-----------------------------------------------------------}
  TALAVLStringListBinaryTree = class(TALStringKeyAVLBinaryTree)
  private
    FEmptyNode: TALAVLStringListBinaryTreeNode; // we use this because inherited Tstring.setvalue use i := add('') following by put(i, avalue)
  protected
    Procedure InternalClear; override;
  public
    Constructor Create; override;
    function    AddNode(aNode: TALAVLStringListBinaryTreeNode): Boolean; reintroduce; virtual;
    function    DeleteNode(idVal: String): boolean; override;
    function    FindNode(idVal: String): TALAVLStringListBinaryTreeNode; Reintroduce; virtual;
  end;

  {-----------------------------------------------------}
  TALAVLStringListBinaryTreeNodeList = class(TObjectList)
  private
  protected
    function GetItem(Index: Integer): TALAVLStringListBinaryTreeNode;
    procedure SetItem(Index: Integer; AObject: TALAVLStringListBinaryTreeNode);
  public
    property Items[Index: Integer]: TALAVLStringListBinaryTreeNode read GetItem write SetItem; default;
  end;

  {--------------------------------}
  TALAVLStringList = class(TStrings)
  private
    FNodeList: TALAVLStringListBinaryTreeNodeList;
    FAVLBinTree: TALAVLStringListBinaryTree;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure QuickSort(L, R: Integer; SCompare: TALAVLStringListSortCompare);
    procedure SetCaseSensitive(const Value: Boolean);
    function GetCaseSensitive: Boolean;
    Function ExtractNameValue(S: String; var Name, Value: String): Boolean;
    function GetForceValue(const Name: string): string;
    function GetForceValueFromIndex(Index: Integer): string;
    procedure SetForceValue(const Name, Value: string);
    procedure SetForceValueFromIndex(Index: Integer; const Value: string);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
    procedure InsertItem(Index: Integer; const S: string; AObject: TObject); virtual;
  public
    Constructor Create; Virtual;
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function IndexOf(const S: string): Integer; override;
    function IndexOfName(const Name: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure InsertObject(Index: Integer; const S: string; AObject: TObject); override;
    procedure CustomSort(Compare: TALAVLStringListSortCompare); virtual;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property ForceValues[const Name: string]: string read GetForceValue write SetForceValue;
    property ForceValueFromIndex[Index: Integer]: string read GetForceValueFromIndex write SetForceValueFromIndex;
  end;

implementation

Uses RTLConsts,
     AlFcnString;

/////////////////////////
///// TALStringList /////
/////////////////////////

{*******************************************************************}
function TALStringList.CompareStrings(const S1, S2: string): Integer;
begin
  //the difference between TALStringList and TStringList is that
  //TstringList use ansiCompareStr or ansiCompareText that are
  //dependant from the local
  if CaseSensitive then Result := CompareStr(
                                             AlStringReplace(S1,NameValueSeparator,#1,[]),
                                             AlStringReplace(S2,NameValueSeparator,#1,[])
                                            )
  else Result := CompareText(
                             AlStringReplace(S1,NameValueSeparator,#1,[]),
                             AlStringReplace(S2,NameValueSeparator,#1,[])
                            );
end;

{****************************************************************************}
function TALStringList.FindName(const S: string; var Index: Integer): Boolean;

  {----------------------------------------------------}
  function InternalExtractName(const S: string): string;
  var P: Integer;
  begin
    Result := S;
    P := AlCharPos(NameValueSeparator, Result);
    if P <> 0 then SetLength(Result, P-1);
  end;

var L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := CompareStrings(InternalExtractName(Get(I)), S);
    if C < 0 then L := I + 1
    else begin
      H := I - 1;
      if C = 0 then Result := True;
    end;
  end;
  Index := L;
end;

{**************************************************************}
function TALStringList.IndexOfName(const Name: string): Integer;
begin
  if not Sorted then Result := inherited IndexOfName(Name)
  else if not FindName(Name, Result) then Result := -1;
end;

{***********************************************************}
procedure TALStringList.Put(Index: Integer; const S: string);
begin
  If not sorted then inherited Put(Index, S)
  else begin
    delete(index);
    add(s);
  end;
end;

{***************************************************************}
function TALStringList.GetForceValue(const Name: string): string;
begin
  Result := Values[Name];
end;

{********************************************************************}
function TALStringList.GetForceValueFromIndex(Index: Integer): string;
begin
  Result := ValueFromIndex[Index];
end;

{***************************************************************}
procedure TALStringList.SetForceValue(const Name, Value: string);
var I: Integer;
begin
  I := IndexOfName(Name);
  if I < 0 then Add(Name + NameValueSeparator + Value)
  else Put(I, Name + NameValueSeparator + Value);
end;

{**********************************************************************************}
procedure TALStringList.SetForceValueFromIndex(Index: Integer; const Value: string);
begin
  if Index < 0 then Add(NameValueSeparator + Value)
  else Put(Index, Names[Index] + NameValueSeparator + Value);
end;




////////////////////////////
///// TALAVLStringList /////
////////////////////////////

{**********************************}
constructor TALAVLStringList.Create;
begin
  FAVLBinTree:= TALAVLStringListBinaryTree.Create;
  FAVLBinTree.CaseSensitive := False;
  FNodeList := TALAVLStringListBinaryTreeNodeList.Create(False);
  FOnChange := nil;
  FOnChanging := nil;
end;

{**********************************}
destructor TALAVLStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  FAVLBinTree.free;
  FNodeList.free;
  inherited Destroy;
end;

{******************************************************}
function TALAVLStringList.Add(const S: string): Integer;
begin
  Result := AddObject(S, nil);
end;

{******************************************************************************}
function TALAVLStringList.AddObject(const S: string; AObject: TObject): Integer;
begin
  Result := Count;
  InsertItem(Result, S, AObject);
end;

{*********************************}
procedure TALAVLStringList.Changed;
begin
  if (UpdateCount = 0) and Assigned(FOnChange) then FOnChange(Self);
end;

{**********************************}
procedure TALAVLStringList.Changing;
begin
  if (UpdateCount = 0) and Assigned(FOnChanging) then FOnChanging(Self);
end;

{*******************************}
procedure TALAVLStringList.Clear;
begin
  if Count <> 0 then begin
    Changing;
    FnodeList.Clear;
    FAVLBinTree.Clear;
    Changed;
  end;
end;

{************************************************}
procedure TALAVLStringList.Delete(Index: Integer);
var i: integer;
begin
  if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
  Changing;
  FAVLBinTree.DeleteNode(FNodelist[Index].ID);
  FNodelist.Delete(Index);
  for i := Index to FNodeList.Count - 1 do
    dec(FNodelist[i].Idx);
  Changed;
end;

{***********************************************************}
procedure TALAVLStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= Count) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= Count) then Error(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

{****************************************************************}
procedure TALAVLStringList.ExchangeItems(Index1, Index2: Integer);
var Item1, Item2: TALAVLStringListBinaryTreeNode;
begin
  Item1 := FNodelist[Index1];
  Item2 := FNodelist[Index2];
  FNodeList.Exchange(Index1,Index2);
  Item1.idx := Index2;
  Item2.idx := Index1;
end;

{****************************************************}
function TALAVLStringList.Get(Index: Integer): string;
begin
  if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
  with FNodelist[Index] do begin
    if Nvs then Result := ID + NameValueSeparator + Val
    else Result := ID;
  end;
end;

{******************************************}
function TALAVLStringList.GetCount: Integer;
begin
  Result := FNodeList.Count;
end;

{******************************************************************}
function TALAVLStringList.GetForceValue(const Name: string): string;
begin
  Result := Values[Name];
end;

{***********************************************************************}
function TALAVLStringList.GetForceValueFromIndex(Index: Integer): string;
begin
  Result := ValueFromIndex[Index];
end;

{******************************************************************}
procedure TALAVLStringList.SetForceValue(const Name, Value: string);
var I: Integer;
begin
  I := IndexOfName(Name);
  if I < 0 then Add(Name + NameValueSeparator + Value)
  else Put(I, Name + NameValueSeparator + Value);
end;

{*************************************************************************************}
procedure TALAVLStringList.SetForceValueFromIndex(Index: Integer; const Value: string);
begin
  if Index < 0 then Add(NameValueSeparator + Value)
  else Put(Index, Names[Index] + NameValueSeparator + Value);
end;

{***********************************************************}
function TALAVLStringList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
  Result := FNodelist[Index].Obj;
end;

{**********************************************************}
function TALAVLStringList.IndexOf(const S: string): Integer;
Var aName, aValue: String;
    aNode: TALAVLStringListBinaryTreeNode;
begin
  if ExtractNameValue(S, aName, aValue) then begin
    aNode := FAVLBinTree.FindNode(aName);
    if (not assigned(aNode)) or
       (
        CaseSensitive and
        (aNode.Val <> aValue)
       )
       or
       (
        (not CaseSensitive) and
        (not sametext(aNode.Val, aValue))
       )
    then result := -1
    else result := aNode.idx;
  end
  else begin
    aNode := FAVLBinTree.FindNode(S);
    if (not assigned(aNode)) or (aNode.Nvs) then result := -1
    else result := aNode.idx;
  end;
end;

{*****************************************************************}
function TALAVLStringList.IndexOfName(const Name: string): Integer;
Var aNode: TALAVLStringListBinaryTreeNode;
begin
  aNode := FAVLBinTree.FindNode(Name);
  if assigned(aNode) and aNode.Nvs then result := aNode.Idx
  else result := -1;
end;

{*****************************************************************}
procedure TALAVLStringList.Insert(Index: Integer; const S: string);
begin
  InsertObject(Index, S, nil);
end;

{*****************************************************************************************}
procedure TALAVLStringList.InsertObject(Index: Integer; const S: string; AObject: TObject);
begin
  if (Index < 0) or (Index > Count) then Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

{***************************************************************************************}
procedure TALAVLStringList.InsertItem(Index: Integer; const S: string; AObject: TObject);
Var aName, AValue: String;
    aNvs: Boolean;
    aNode: TALAVLStringListBinaryTreeNode;
    i: integer;
begin
  Changing;

  if ExtractNameValue(S, aName, aValue) then aNvs := True
  else begin
    aName := S;
    aValue := '';
    aNvs := False;
  end;

  aNode := TALAVLStringListBinaryTreeNode.Create;
  aNode.Idx := Index;
  aNode.ID := aName;
  aNode.Val := aValue;
  aNode.Nvs := aNvs;
  aNode.Obj := AObject;
  if not FAVLBinTree.AddNode(aNode) then begin
    aNode.free;
    Raise Exception.create('List does not allow duplicate Names');
  end;
  FNodeList.Insert(Index, aNode);
  for i := Index + 1 to FNodeList.Count - 1 do
    inc(FNodelist[i].Idx);

  Changed;
end;

{**************************************************************}
procedure TALAVLStringList.Put(Index: Integer; const S: string);
Var aNewName, aNewValue: String;
    aNewNvs: Boolean;
    aNewNode, aOldNode: TALAVLStringListBinaryTreeNode;
begin
  if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
  Changing;

  if ExtractNameValue(S, aNewName, aNewValue) then aNewNvs := True
  else begin
    aNewName := S;
    aNewValue := '';
    aNewNvs := False;
  end;

  aOldNode := FNodeList[index];

  if (CaseSensitive and (aOldNode.ID <> aNewName)) or
     ((not CaseSensitive) and (not sametext(aOldNode.ID, aNewName))) then begin
    aNewNode := TALAVLStringListBinaryTreeNode.Create;
    aNewNode.Idx := Index;
    aNewNode.ID := aNewName;
    aNewNode.Val := ANewValue;
    aNewNode.NVS := aNewNvs;
    aNewNode.Obj := aOldNode.Obj;
    if not FAVLBinTree.AddNode(aNewNode) then begin
      aNewNode.free;
      Raise Exception.create('List does not allow duplicate Names');
    end;
    FNodeList[Index] := aNewNode;
    FAVLBinTree.DeleteNode(aOldNode.ID);
  end
  else begin
    aOldNode.Val := aNewValue;
    aOldNode.NVS := aNewNVS;
  end;

  Changed;
end;

{*********************************************************************}
procedure TALAVLStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
  Changing;
  FNodeList[Index].Obj := AObject;
  Changed;
end;

{*****************************************************************************************}
procedure TALAVLStringList.QuickSort(L, R: Integer; SCompare: TALAVLStringListSortCompare);
var I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then begin
        ExchangeItems(I, J);
        if P = I then P := J
        else if P = J then P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

{***********************************************************}
procedure TALAVLStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing
  else Changed;
end;

{**************************************************************************}
procedure TALAVLStringList.CustomSort(Compare: TALAVLStringListSortCompare);
begin
  if (Count > 1) then begin
    Changing;
    QuickSort(0, Count - 1, Compare);
    Changed;
  end;
end;

{**************************************************}
function TALAVLStringList.GetCaseSensitive: Boolean;
begin
  result := FAVLBinTree.CaseSensitive;
end;

{****************************************************************}
procedure TALAVLStringList.SetCaseSensitive(const Value: Boolean);
begin
  FAVLBinTree.CaseSensitive := Value;
end;

{**************************************************************************************}
Function TALAVLStringList.ExtractNameValue(S: String; var Name, Value: String): Boolean;
Var P1: Integer;
begin
  P1 := AlPos(NameValueSeparator,S);
  if P1 > 0 then begin
    result := True;
    Name := AlCopyStr(S,1,P1-1);
    Value := AlCopyStr(S,P1+1, maxint);
  end
  else begin
    Result := False;
    Name := '';
    Value := '';
  end;
end;




////////////////////////////////
///// TALAVLStringListNode /////
////////////////////////////////

{************************************************}
constructor TALAVLStringListBinaryTreeNode.Create;
begin
  inherited;
  Val := '';
  Obj := nil;
  idx := -1;
  NVS := False;
end;




//////////////////////////////////////
///// TALAVLStringListBinaryTree /////
//////////////////////////////////////

{********************************************}
constructor TALAVLStringListBinaryTree.Create;
begin
  inherited;
  FEmptyNode := nil;
end;

{******************************************************************************************}
function TALAVLStringListBinaryTree.AddNode(aNode: TALAVLStringListBinaryTreeNode): Boolean;
begin
  if (aNode.ID = '') then begin
    Result := not Assigned(fEmptyNode);
    if Result then FemptyNode := aNode;
  end
  else Result := inherited addNode(aNode);
end;

{*********************************************************************}
function TALAVLStringListBinaryTree.DeleteNode(idVal: String): boolean;
begin
  if (IDVal = '') then begin
    Result := Assigned(fEmptyNode);
    if result then FreeAndNil(fEmptyNode);
  end
  else result := inherited DeleteNode(idVal);
end;

{******************************************************************************************}
function TALAVLStringListBinaryTree.FindNode(idVal: String): TALAVLStringListBinaryTreeNode;
var aNode: TALStringKeyAVLBinaryTreeNode;
begin
  if (IDVal = '') then result := FEmptyNode
  else begin
    aNode := Inherited FindNode(IdVal);
    if assigned(aNode) then result := TALAVLStringListBinaryTreeNode(aNode)
    else result := nil;
  end;
end;

{*************************************************}
procedure TALAVLStringListBinaryTree.InternalClear;
begin
  inherited;
  if assigned(FEmptyNode) then FreeAndNil(FEmptyNode);
end;




//////////////////////////////////////////////
///// TALAVLStringListBinaryTreeNodeList /////
//////////////////////////////////////////////

{**************************************************************************************************}
function TALAVLStringListBinaryTreeNodeList.GetItem(Index: Integer): TALAVLStringListBinaryTreeNode;
begin
  Result := TALAVLStringListBinaryTreeNode(inherited Items[Index]);
end;

{************************************************************************************************************}
procedure TALAVLStringListBinaryTreeNodeList.SetItem(Index: Integer; AObject: TALAVLStringListBinaryTreeNode);
begin
  inherited Items[Index] := AObject;
end;

end.



