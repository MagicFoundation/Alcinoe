{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclTrees.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet. Portions created by                }
{ Florent Ouchet are Copyright (C) Florent Ouchet <outchy att users dott sourceforge dott net      }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclTrees;

interface

{$I jcl.inc}

uses
  {$IFDEF HAS_UNITSCOPE}
  System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF SUPPORTS_GENERICS}
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclContainerIntf, JclSynch;


type
  TItrStart = (isFirst, isLast, isRoot);

  TJclIntfTreeNode = class
  public
    Value: IInterface;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclIntfTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclIntfTreeNode;
    function IndexOfChild(AChild: TJclIntfTreeNode): Integer;
    function IndexOfValue(const AInterface: IInterface; const AEqualityComparer: IJclIntfEqualityComparer): Integer;
  end;

  TJclIntfTree = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclIntfContainer, IJclIntfFlatContainer, IJclIntfEqualityComparer,
    IJclIntfCollection, IJclIntfTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FRoot: TJclIntfTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ExtractNode(var ANode: TJclIntfTreeNode);
    procedure RemoveNode(var ANode: TJclIntfTreeNode);
    function CloneNode(Node, Parent: TJclIntfTreeNode): TJclIntfTreeNode;
    function NodeContains(ANode: TJclIntfTreeNode; const AInterface: IInterface): Boolean;
    procedure PackNode(ANode: TJclIntfTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create();
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCollection }
    function Add(const AInterface: IInterface): Boolean;
    function AddAll(const ACollection: IJclIntfCollection): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclIntfCollection): Boolean;
    function Contains(const AInterface: IInterface): Boolean;
    function ContainsAll(const ACollection: IJclIntfCollection): Boolean;
    function Extract(const AInterface: IInterface): Boolean;
    function ExtractAll(const ACollection: IJclIntfCollection): Boolean;
    function First: IJclIntfIterator;
    function IsEmpty: Boolean;
    function Last: IJclIntfIterator;
    function Remove(const AInterface: IInterface): Boolean;
    function RemoveAll(const ACollection: IJclIntfCollection): Boolean;
    function RetainAll(const ACollection: IJclIntfCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIntfIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntfTree }
    function GetRoot: IJclIntfTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclIntfTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclIntfTreeIterator = class(TJclAbstractIterator, IJclIntfIterator, IJclIntfTreeIterator)
  protected
    FCursor: TJclIntfTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclIntfTree;
    FEqualityComparer: IJclIntfEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclIntfTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclIntfTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclIntfTreeNode; virtual; abstract;
  public
    constructor Create(OwnTree: TJclIntfTree; ACursor: TJclIntfTreeNode; AValid: Boolean; AStart: TItrStart);
    { IJclIntfIterator }
    function Add(const AInterface: IInterface): Boolean;
    procedure Extract;
    function GetObject: IInterface;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AInterface: IInterface): Boolean;
    function IteratorEquals(const AIterator: IJclIntfIterator): Boolean;
    function Next: IInterface;
    function NextIndex: Integer;
    function Previous: IInterface;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetObject(const AInterface: IInterface);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: IInterface read GetObject;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntfTreeIterator }
    function AddChild(const AInterface: IInterface): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): IInterface;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AInterface: IInterface): Integer;
    function InsertChild(Index: Integer; const AInterface: IInterface): Boolean;
    function Parent: IInterface;
    procedure SetChild(Index: Integer; const AInterface: IInterface);
  end;

  TJclPreOrderIntfTreeIterator = class(TJclIntfTreeIterator, IJclIntfIterator, IJclIntfTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntfTreeNode; override;
    function GetNextSibling: TJclIntfTreeNode; override;
    function GetPreviousCursor: TJclIntfTreeNode; override;
  end;

  TJclPostOrderIntfTreeIterator = class(TJclIntfTreeIterator, IJclIntfIterator, IJclIntfTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntfTreeNode; override;
    function GetNextSibling: TJclIntfTreeNode; override;
    function GetPreviousCursor: TJclIntfTreeNode; override;
  end;

  TJclAnsiStrTreeNode = class
  public
    Value: AnsiString;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclAnsiStrTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclAnsiStrTreeNode;
    function IndexOfChild(AChild: TJclAnsiStrTreeNode): Integer;
    function IndexOfValue(const AString: AnsiString; const AEqualityComparer: IJclAnsiStrEqualityComparer): Integer;
  end;

  TJclAnsiStrTree = class(TJclAnsiStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclAnsiStrContainer, IJclAnsiStrFlatContainer, IJclAnsiStrEqualityComparer, IJclStrBaseContainer,
    IJclAnsiStrCollection, IJclAnsiStrTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FRoot: TJclAnsiStrTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ExtractNode(var ANode: TJclAnsiStrTreeNode);
    procedure RemoveNode(var ANode: TJclAnsiStrTreeNode);
    function CloneNode(Node, Parent: TJclAnsiStrTreeNode): TJclAnsiStrTreeNode;
    function NodeContains(ANode: TJclAnsiStrTreeNode; const AString: AnsiString): Boolean;
    procedure PackNode(ANode: TJclAnsiStrTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create();
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclAnsiStrCollection }
    function Add(const AString: AnsiString): Boolean; override;
    function AddAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    procedure Clear; override;
    function CollectionEquals(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Contains(const AString: AnsiString): Boolean; override;
    function ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Extract(const AString: AnsiString): Boolean; override;
    function ExtractAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function First: IJclAnsiStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclAnsiStrIterator; override;
    function Remove(const AString: AnsiString): Boolean; override;
    function RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Size: Integer; override;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclAnsiStrIterator; override;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclAnsiStrTree }
    function GetRoot: IJclAnsiStrTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclAnsiStrTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclAnsiStrTreeIterator = class(TJclAbstractIterator, IJclAnsiStrIterator, IJclAnsiStrTreeIterator)
  protected
    FCursor: TJclAnsiStrTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclAnsiStrTree;
    FEqualityComparer: IJclAnsiStrEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclAnsiStrTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclAnsiStrTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclAnsiStrTreeNode; virtual; abstract;
  public
    constructor Create(OwnTree: TJclAnsiStrTree; ACursor: TJclAnsiStrTreeNode; AValid: Boolean; AStart: TItrStart);
    { IJclAnsiStrIterator }
    function Add(const AString: AnsiString): Boolean;
    procedure Extract;
    function GetString: AnsiString;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AString: AnsiString): Boolean;
    function IteratorEquals(const AIterator: IJclAnsiStrIterator): Boolean;
    function Next: AnsiString;
    function NextIndex: Integer;
    function Previous: AnsiString;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetString(const AString: AnsiString);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: AnsiString read GetString;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclAnsiStrTreeIterator }
    function AddChild(const AString: AnsiString): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): AnsiString;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AString: AnsiString): Integer;
    function InsertChild(Index: Integer; const AString: AnsiString): Boolean;
    function Parent: AnsiString;
    procedure SetChild(Index: Integer; const AString: AnsiString);
  end;

  TJclPreOrderAnsiStrTreeIterator = class(TJclAnsiStrTreeIterator, IJclAnsiStrIterator, IJclAnsiStrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclAnsiStrTreeNode; override;
    function GetNextSibling: TJclAnsiStrTreeNode; override;
    function GetPreviousCursor: TJclAnsiStrTreeNode; override;
  end;

  TJclPostOrderAnsiStrTreeIterator = class(TJclAnsiStrTreeIterator, IJclAnsiStrIterator, IJclAnsiStrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclAnsiStrTreeNode; override;
    function GetNextSibling: TJclAnsiStrTreeNode; override;
    function GetPreviousCursor: TJclAnsiStrTreeNode; override;
  end;

  TJclWideStrTreeNode = class
  public
    Value: WideString;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclWideStrTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclWideStrTreeNode;
    function IndexOfChild(AChild: TJclWideStrTreeNode): Integer;
    function IndexOfValue(const AString: WideString; const AEqualityComparer: IJclWideStrEqualityComparer): Integer;
  end;

  TJclWideStrTree = class(TJclWideStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclWideStrContainer, IJclWideStrFlatContainer, IJclWideStrEqualityComparer, IJclStrBaseContainer,
    IJclWideStrCollection, IJclWideStrTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FRoot: TJclWideStrTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ExtractNode(var ANode: TJclWideStrTreeNode);
    procedure RemoveNode(var ANode: TJclWideStrTreeNode);
    function CloneNode(Node, Parent: TJclWideStrTreeNode): TJclWideStrTreeNode;
    function NodeContains(ANode: TJclWideStrTreeNode; const AString: WideString): Boolean;
    procedure PackNode(ANode: TJclWideStrTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create();
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclWideStrCollection }
    function Add(const AString: WideString): Boolean; override;
    function AddAll(const ACollection: IJclWideStrCollection): Boolean; override;
    procedure Clear; override;
    function CollectionEquals(const ACollection: IJclWideStrCollection): Boolean; override;
    function Contains(const AString: WideString): Boolean; override;
    function ContainsAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function Extract(const AString: WideString): Boolean; override;
    function ExtractAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function First: IJclWideStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclWideStrIterator; override;
    function Remove(const AString: WideString): Boolean; override;
    function RemoveAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function Size: Integer; override;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclWideStrIterator; override;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclWideStrTree }
    function GetRoot: IJclWideStrTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclWideStrTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclWideStrTreeIterator = class(TJclAbstractIterator, IJclWideStrIterator, IJclWideStrTreeIterator)
  protected
    FCursor: TJclWideStrTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclWideStrTree;
    FEqualityComparer: IJclWideStrEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclWideStrTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclWideStrTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclWideStrTreeNode; virtual; abstract;
  public
    constructor Create(OwnTree: TJclWideStrTree; ACursor: TJclWideStrTreeNode; AValid: Boolean; AStart: TItrStart);
    { IJclWideStrIterator }
    function Add(const AString: WideString): Boolean;
    procedure Extract;
    function GetString: WideString;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AString: WideString): Boolean;
    function IteratorEquals(const AIterator: IJclWideStrIterator): Boolean;
    function Next: WideString;
    function NextIndex: Integer;
    function Previous: WideString;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetString(const AString: WideString);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: WideString read GetString;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclWideStrTreeIterator }
    function AddChild(const AString: WideString): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): WideString;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AString: WideString): Integer;
    function InsertChild(Index: Integer; const AString: WideString): Boolean;
    function Parent: WideString;
    procedure SetChild(Index: Integer; const AString: WideString);
  end;

  TJclPreOrderWideStrTreeIterator = class(TJclWideStrTreeIterator, IJclWideStrIterator, IJclWideStrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclWideStrTreeNode; override;
    function GetNextSibling: TJclWideStrTreeNode; override;
    function GetPreviousCursor: TJclWideStrTreeNode; override;
  end;

  TJclPostOrderWideStrTreeIterator = class(TJclWideStrTreeIterator, IJclWideStrIterator, IJclWideStrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclWideStrTreeNode; override;
    function GetNextSibling: TJclWideStrTreeNode; override;
    function GetPreviousCursor: TJclWideStrTreeNode; override;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrTreeNode = class
  public
    Value: UnicodeString;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclUnicodeStrTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclUnicodeStrTreeNode;
    function IndexOfChild(AChild: TJclUnicodeStrTreeNode): Integer;
    function IndexOfValue(const AString: UnicodeString; const AEqualityComparer: IJclUnicodeStrEqualityComparer): Integer;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrTree = class(TJclUnicodeStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclUnicodeStrContainer, IJclUnicodeStrFlatContainer, IJclUnicodeStrEqualityComparer, IJclStrBaseContainer,
    IJclUnicodeStrCollection, IJclUnicodeStrTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FRoot: TJclUnicodeStrTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ExtractNode(var ANode: TJclUnicodeStrTreeNode);
    procedure RemoveNode(var ANode: TJclUnicodeStrTreeNode);
    function CloneNode(Node, Parent: TJclUnicodeStrTreeNode): TJclUnicodeStrTreeNode;
    function NodeContains(ANode: TJclUnicodeStrTreeNode; const AString: UnicodeString): Boolean;
    procedure PackNode(ANode: TJclUnicodeStrTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create();
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclUnicodeStrCollection }
    function Add(const AString: UnicodeString): Boolean; override;
    function AddAll(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    procedure Clear; override;
    function CollectionEquals(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    function Contains(const AString: UnicodeString): Boolean; override;
    function ContainsAll(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    function Extract(const AString: UnicodeString): Boolean; override;
    function ExtractAll(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    function First: IJclUnicodeStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclUnicodeStrIterator; override;
    function Remove(const AString: UnicodeString): Boolean; override;
    function RemoveAll(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    function Size: Integer; override;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclUnicodeStrIterator; override;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclUnicodeStrTree }
    function GetRoot: IJclUnicodeStrTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclUnicodeStrTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrTreeIterator = class(TJclAbstractIterator, IJclUnicodeStrIterator, IJclUnicodeStrTreeIterator)
  protected
    FCursor: TJclUnicodeStrTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclUnicodeStrTree;
    FEqualityComparer: IJclUnicodeStrEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclUnicodeStrTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclUnicodeStrTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclUnicodeStrTreeNode; virtual; abstract;
  public
    constructor Create(OwnTree: TJclUnicodeStrTree; ACursor: TJclUnicodeStrTreeNode; AValid: Boolean; AStart: TItrStart);
    { IJclUnicodeStrIterator }
    function Add(const AString: UnicodeString): Boolean;
    procedure Extract;
    function GetString: UnicodeString;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AString: UnicodeString): Boolean;
    function IteratorEquals(const AIterator: IJclUnicodeStrIterator): Boolean;
    function Next: UnicodeString;
    function NextIndex: Integer;
    function Previous: UnicodeString;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetString(const AString: UnicodeString);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: UnicodeString read GetString;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclUnicodeStrTreeIterator }
    function AddChild(const AString: UnicodeString): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): UnicodeString;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AString: UnicodeString): Integer;
    function InsertChild(Index: Integer; const AString: UnicodeString): Boolean;
    function Parent: UnicodeString;
    procedure SetChild(Index: Integer; const AString: UnicodeString);
  end;

  TJclPreOrderUnicodeStrTreeIterator = class(TJclUnicodeStrTreeIterator, IJclUnicodeStrIterator, IJclUnicodeStrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclUnicodeStrTreeNode; override;
    function GetNextSibling: TJclUnicodeStrTreeNode; override;
    function GetPreviousCursor: TJclUnicodeStrTreeNode; override;
  end;

  TJclPostOrderUnicodeStrTreeIterator = class(TJclUnicodeStrTreeIterator, IJclUnicodeStrIterator, IJclUnicodeStrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclUnicodeStrTreeNode; override;
    function GetNextSibling: TJclUnicodeStrTreeNode; override;
    function GetPreviousCursor: TJclUnicodeStrTreeNode; override;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrTreeNode = TJclAnsiStrTreeNode;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrTreeNode = TJclWideStrTreeNode;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrTreeNode = TJclUnicodeStrTreeNode;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrTree = TJclAnsiStrTree;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrTree = TJclWideStrTree;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrTree = TJclUnicodeStrTree;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrTreeIterator = TJclAnsiStrTreeIterator;
  TJclPreOrderStrTreeIterator = TJclPreOrderAnsiStrTreeIterator;
  TJclPostOrderStrTreeIterator = TJclPostOrderAnsiStrTreeIterator;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrTreeIterator = TJclWideStrTreeIterator;
  TJclPreOrderStrTreeIterator = TJclPreOrderWideStrTreeIterator;
  TJclPostOrderStrTreeIterator = TJclPostOrderWideStrTreeIterator;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrTreeIterator = TJclUnicodeStrTreeIterator;
  TJclPreOrderStrTreeIterator = TJclPreOrderUnicodeStrTreeIterator;
  TJclPostOrderStrTreeIterator = TJclPostOrderUnicodeStrTreeIterator;
  {$ENDIF CONTAINER_UNICODESTR}

  TJclSingleTreeNode = class
  public
    Value: Single;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclSingleTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclSingleTreeNode;
    function IndexOfChild(AChild: TJclSingleTreeNode): Integer;
    function IndexOfValue(const AValue: Single; const AEqualityComparer: IJclSingleEqualityComparer): Integer;
  end;

  TJclSingleTree = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclSingleContainer, IJclSingleFlatContainer, IJclSingleEqualityComparer,
    IJclSingleCollection, IJclSingleTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FRoot: TJclSingleTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ExtractNode(var ANode: TJclSingleTreeNode);
    procedure RemoveNode(var ANode: TJclSingleTreeNode);
    function CloneNode(Node, Parent: TJclSingleTreeNode): TJclSingleTreeNode;
    function NodeContains(ANode: TJclSingleTreeNode; const AValue: Single): Boolean;
    procedure PackNode(ANode: TJclSingleTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create();
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclSingleCollection }
    function Add(const AValue: Single): Boolean;
    function AddAll(const ACollection: IJclSingleCollection): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclSingleCollection): Boolean;
    function Contains(const AValue: Single): Boolean;
    function ContainsAll(const ACollection: IJclSingleCollection): Boolean;
    function Extract(const AValue: Single): Boolean;
    function ExtractAll(const ACollection: IJclSingleCollection): Boolean;
    function First: IJclSingleIterator;
    function IsEmpty: Boolean;
    function Last: IJclSingleIterator;
    function Remove(const AValue: Single): Boolean;
    function RemoveAll(const ACollection: IJclSingleCollection): Boolean;
    function RetainAll(const ACollection: IJclSingleCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclSingleIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclSingleTree }
    function GetRoot: IJclSingleTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclSingleTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclSingleTreeIterator = class(TJclAbstractIterator, IJclSingleIterator, IJclSingleTreeIterator)
  protected
    FCursor: TJclSingleTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclSingleTree;
    FEqualityComparer: IJclSingleEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclSingleTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclSingleTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclSingleTreeNode; virtual; abstract;
  public
    constructor Create(OwnTree: TJclSingleTree; ACursor: TJclSingleTreeNode; AValid: Boolean; AStart: TItrStart);
    { IJclSingleIterator }
    function Add(const AValue: Single): Boolean;
    procedure Extract;
    function GetValue: Single;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Single): Boolean;
    function IteratorEquals(const AIterator: IJclSingleIterator): Boolean;
    function Next: Single;
    function NextIndex: Integer;
    function Previous: Single;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(const AValue: Single);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Single read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclSingleTreeIterator }
    function AddChild(const AValue: Single): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): Single;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AValue: Single): Integer;
    function InsertChild(Index: Integer; const AValue: Single): Boolean;
    function Parent: Single;
    procedure SetChild(Index: Integer; const AValue: Single);
  end;

  TJclPreOrderSingleTreeIterator = class(TJclSingleTreeIterator, IJclSingleIterator, IJclSingleTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclSingleTreeNode; override;
    function GetNextSibling: TJclSingleTreeNode; override;
    function GetPreviousCursor: TJclSingleTreeNode; override;
  end;

  TJclPostOrderSingleTreeIterator = class(TJclSingleTreeIterator, IJclSingleIterator, IJclSingleTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclSingleTreeNode; override;
    function GetNextSibling: TJclSingleTreeNode; override;
    function GetPreviousCursor: TJclSingleTreeNode; override;
  end;

  TJclDoubleTreeNode = class
  public
    Value: Double;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclDoubleTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclDoubleTreeNode;
    function IndexOfChild(AChild: TJclDoubleTreeNode): Integer;
    function IndexOfValue(const AValue: Double; const AEqualityComparer: IJclDoubleEqualityComparer): Integer;
  end;

  TJclDoubleTree = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclDoubleContainer, IJclDoubleFlatContainer, IJclDoubleEqualityComparer,
    IJclDoubleCollection, IJclDoubleTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FRoot: TJclDoubleTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ExtractNode(var ANode: TJclDoubleTreeNode);
    procedure RemoveNode(var ANode: TJclDoubleTreeNode);
    function CloneNode(Node, Parent: TJclDoubleTreeNode): TJclDoubleTreeNode;
    function NodeContains(ANode: TJclDoubleTreeNode; const AValue: Double): Boolean;
    procedure PackNode(ANode: TJclDoubleTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create();
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclDoubleCollection }
    function Add(const AValue: Double): Boolean;
    function AddAll(const ACollection: IJclDoubleCollection): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclDoubleCollection): Boolean;
    function Contains(const AValue: Double): Boolean;
    function ContainsAll(const ACollection: IJclDoubleCollection): Boolean;
    function Extract(const AValue: Double): Boolean;
    function ExtractAll(const ACollection: IJclDoubleCollection): Boolean;
    function First: IJclDoubleIterator;
    function IsEmpty: Boolean;
    function Last: IJclDoubleIterator;
    function Remove(const AValue: Double): Boolean;
    function RemoveAll(const ACollection: IJclDoubleCollection): Boolean;
    function RetainAll(const ACollection: IJclDoubleCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclDoubleIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclDoubleTree }
    function GetRoot: IJclDoubleTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclDoubleTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclDoubleTreeIterator = class(TJclAbstractIterator, IJclDoubleIterator, IJclDoubleTreeIterator)
  protected
    FCursor: TJclDoubleTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclDoubleTree;
    FEqualityComparer: IJclDoubleEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclDoubleTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclDoubleTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclDoubleTreeNode; virtual; abstract;
  public
    constructor Create(OwnTree: TJclDoubleTree; ACursor: TJclDoubleTreeNode; AValid: Boolean; AStart: TItrStart);
    { IJclDoubleIterator }
    function Add(const AValue: Double): Boolean;
    procedure Extract;
    function GetValue: Double;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Double): Boolean;
    function IteratorEquals(const AIterator: IJclDoubleIterator): Boolean;
    function Next: Double;
    function NextIndex: Integer;
    function Previous: Double;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(const AValue: Double);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Double read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclDoubleTreeIterator }
    function AddChild(const AValue: Double): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): Double;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AValue: Double): Integer;
    function InsertChild(Index: Integer; const AValue: Double): Boolean;
    function Parent: Double;
    procedure SetChild(Index: Integer; const AValue: Double);
  end;

  TJclPreOrderDoubleTreeIterator = class(TJclDoubleTreeIterator, IJclDoubleIterator, IJclDoubleTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclDoubleTreeNode; override;
    function GetNextSibling: TJclDoubleTreeNode; override;
    function GetPreviousCursor: TJclDoubleTreeNode; override;
  end;

  TJclPostOrderDoubleTreeIterator = class(TJclDoubleTreeIterator, IJclDoubleIterator, IJclDoubleTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclDoubleTreeNode; override;
    function GetNextSibling: TJclDoubleTreeNode; override;
    function GetPreviousCursor: TJclDoubleTreeNode; override;
  end;

  TJclExtendedTreeNode = class
  public
    Value: Extended;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclExtendedTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclExtendedTreeNode;
    function IndexOfChild(AChild: TJclExtendedTreeNode): Integer;
    function IndexOfValue(const AValue: Extended; const AEqualityComparer: IJclExtendedEqualityComparer): Integer;
  end;

  TJclExtendedTree = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclExtendedContainer, IJclExtendedFlatContainer, IJclExtendedEqualityComparer,
    IJclExtendedCollection, IJclExtendedTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FRoot: TJclExtendedTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ExtractNode(var ANode: TJclExtendedTreeNode);
    procedure RemoveNode(var ANode: TJclExtendedTreeNode);
    function CloneNode(Node, Parent: TJclExtendedTreeNode): TJclExtendedTreeNode;
    function NodeContains(ANode: TJclExtendedTreeNode; const AValue: Extended): Boolean;
    procedure PackNode(ANode: TJclExtendedTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create();
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclExtendedCollection }
    function Add(const AValue: Extended): Boolean;
    function AddAll(const ACollection: IJclExtendedCollection): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclExtendedCollection): Boolean;
    function Contains(const AValue: Extended): Boolean;
    function ContainsAll(const ACollection: IJclExtendedCollection): Boolean;
    function Extract(const AValue: Extended): Boolean;
    function ExtractAll(const ACollection: IJclExtendedCollection): Boolean;
    function First: IJclExtendedIterator;
    function IsEmpty: Boolean;
    function Last: IJclExtendedIterator;
    function Remove(const AValue: Extended): Boolean;
    function RemoveAll(const ACollection: IJclExtendedCollection): Boolean;
    function RetainAll(const ACollection: IJclExtendedCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclExtendedIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclExtendedTree }
    function GetRoot: IJclExtendedTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclExtendedTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclExtendedTreeIterator = class(TJclAbstractIterator, IJclExtendedIterator, IJclExtendedTreeIterator)
  protected
    FCursor: TJclExtendedTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclExtendedTree;
    FEqualityComparer: IJclExtendedEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclExtendedTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclExtendedTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclExtendedTreeNode; virtual; abstract;
  public
    constructor Create(OwnTree: TJclExtendedTree; ACursor: TJclExtendedTreeNode; AValid: Boolean; AStart: TItrStart);
    { IJclExtendedIterator }
    function Add(const AValue: Extended): Boolean;
    procedure Extract;
    function GetValue: Extended;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Extended): Boolean;
    function IteratorEquals(const AIterator: IJclExtendedIterator): Boolean;
    function Next: Extended;
    function NextIndex: Integer;
    function Previous: Extended;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(const AValue: Extended);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Extended read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclExtendedTreeIterator }
    function AddChild(const AValue: Extended): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): Extended;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AValue: Extended): Integer;
    function InsertChild(Index: Integer; const AValue: Extended): Boolean;
    function Parent: Extended;
    procedure SetChild(Index: Integer; const AValue: Extended);
  end;

  TJclPreOrderExtendedTreeIterator = class(TJclExtendedTreeIterator, IJclExtendedIterator, IJclExtendedTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclExtendedTreeNode; override;
    function GetNextSibling: TJclExtendedTreeNode; override;
    function GetPreviousCursor: TJclExtendedTreeNode; override;
  end;

  TJclPostOrderExtendedTreeIterator = class(TJclExtendedTreeIterator, IJclExtendedIterator, IJclExtendedTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclExtendedTreeNode; override;
    function GetNextSibling: TJclExtendedTreeNode; override;
    function GetPreviousCursor: TJclExtendedTreeNode; override;
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatTreeNode = TJclSingleTreeNode;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatTreeNode = TJclDoubleTreeNode;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatTreeNode = TJclExtendedTreeNode;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatTree = TJclSingleTree;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatTree = TJclDoubleTree;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatTree = TJclExtendedTree;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatTreeIterator = TJclSingleTreeIterator;
  TJclPreOrderFloatTreeIterator = TJclPreOrderSingleTreeIterator;
  TJclPostOrderFloatTreeIterator = TJclPostOrderSingleTreeIterator;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatTreeIterator = TJclDoubleTreeIterator;
  TJclPreOrderFloatTreeIterator = TJclPreOrderDoubleTreeIterator;
  TJclPostOrderFloatTreeIterator = TJclPostOrderDoubleTreeIterator;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatTreeIterator = TJclExtendedTreeIterator;
  TJclPreOrderFloatTreeIterator = TJclPreOrderExtendedTreeIterator;
  TJclPostOrderFloatTreeIterator = TJclPostOrderExtendedTreeIterator;
  {$ENDIF MATH_EXTENDED_PRECISION}

  TJclIntegerTreeNode = class
  public
    Value: Integer;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclIntegerTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclIntegerTreeNode;
    function IndexOfChild(AChild: TJclIntegerTreeNode): Integer;
    function IndexOfValue(AValue: Integer; const AEqualityComparer: IJclIntegerEqualityComparer): Integer;
  end;

  TJclIntegerTree = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclIntegerContainer, IJclIntegerFlatContainer, IJclIntegerEqualityComparer,
    IJclIntegerCollection, IJclIntegerTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FRoot: TJclIntegerTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ExtractNode(var ANode: TJclIntegerTreeNode);
    procedure RemoveNode(var ANode: TJclIntegerTreeNode);
    function CloneNode(Node, Parent: TJclIntegerTreeNode): TJclIntegerTreeNode;
    function NodeContains(ANode: TJclIntegerTreeNode; AValue: Integer): Boolean;
    procedure PackNode(ANode: TJclIntegerTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create();
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntegerCollection }
    function Add(AValue: Integer): Boolean;
    function AddAll(const ACollection: IJclIntegerCollection): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclIntegerCollection): Boolean;
    function Contains(AValue: Integer): Boolean;
    function ContainsAll(const ACollection: IJclIntegerCollection): Boolean;
    function Extract(AValue: Integer): Boolean;
    function ExtractAll(const ACollection: IJclIntegerCollection): Boolean;
    function First: IJclIntegerIterator;
    function IsEmpty: Boolean;
    function Last: IJclIntegerIterator;
    function Remove(AValue: Integer): Boolean;
    function RemoveAll(const ACollection: IJclIntegerCollection): Boolean;
    function RetainAll(const ACollection: IJclIntegerCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIntegerIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntegerTree }
    function GetRoot: IJclIntegerTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclIntegerTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclIntegerTreeIterator = class(TJclAbstractIterator, IJclIntegerIterator, IJclIntegerTreeIterator)
  protected
    FCursor: TJclIntegerTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclIntegerTree;
    FEqualityComparer: IJclIntegerEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclIntegerTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclIntegerTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclIntegerTreeNode; virtual; abstract;
  public
    constructor Create(OwnTree: TJclIntegerTree; ACursor: TJclIntegerTreeNode; AValid: Boolean; AStart: TItrStart);
    { IJclIntegerIterator }
    function Add(AValue: Integer): Boolean;
    procedure Extract;
    function GetValue: Integer;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(AValue: Integer): Boolean;
    function IteratorEquals(const AIterator: IJclIntegerIterator): Boolean;
    function Next: Integer;
    function NextIndex: Integer;
    function Previous: Integer;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(AValue: Integer);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Integer read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntegerTreeIterator }
    function AddChild(AValue: Integer): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): Integer;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(AValue: Integer): Integer;
    function InsertChild(Index: Integer; AValue: Integer): Boolean;
    function Parent: Integer;
    procedure SetChild(Index: Integer; AValue: Integer);
  end;

  TJclPreOrderIntegerTreeIterator = class(TJclIntegerTreeIterator, IJclIntegerIterator, IJclIntegerTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntegerTreeNode; override;
    function GetNextSibling: TJclIntegerTreeNode; override;
    function GetPreviousCursor: TJclIntegerTreeNode; override;
  end;

  TJclPostOrderIntegerTreeIterator = class(TJclIntegerTreeIterator, IJclIntegerIterator, IJclIntegerTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntegerTreeNode; override;
    function GetNextSibling: TJclIntegerTreeNode; override;
    function GetPreviousCursor: TJclIntegerTreeNode; override;
  end;

  TJclCardinalTreeNode = class
  public
    Value: Cardinal;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclCardinalTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclCardinalTreeNode;
    function IndexOfChild(AChild: TJclCardinalTreeNode): Integer;
    function IndexOfValue(AValue: Cardinal; const AEqualityComparer: IJclCardinalEqualityComparer): Integer;
  end;

  TJclCardinalTree = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclCardinalContainer, IJclCardinalFlatContainer, IJclCardinalEqualityComparer,
    IJclCardinalCollection, IJclCardinalTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FRoot: TJclCardinalTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ExtractNode(var ANode: TJclCardinalTreeNode);
    procedure RemoveNode(var ANode: TJclCardinalTreeNode);
    function CloneNode(Node, Parent: TJclCardinalTreeNode): TJclCardinalTreeNode;
    function NodeContains(ANode: TJclCardinalTreeNode; AValue: Cardinal): Boolean;
    procedure PackNode(ANode: TJclCardinalTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create();
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCardinalCollection }
    function Add(AValue: Cardinal): Boolean;
    function AddAll(const ACollection: IJclCardinalCollection): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclCardinalCollection): Boolean;
    function Contains(AValue: Cardinal): Boolean;
    function ContainsAll(const ACollection: IJclCardinalCollection): Boolean;
    function Extract(AValue: Cardinal): Boolean;
    function ExtractAll(const ACollection: IJclCardinalCollection): Boolean;
    function First: IJclCardinalIterator;
    function IsEmpty: Boolean;
    function Last: IJclCardinalIterator;
    function Remove(AValue: Cardinal): Boolean;
    function RemoveAll(const ACollection: IJclCardinalCollection): Boolean;
    function RetainAll(const ACollection: IJclCardinalCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclCardinalIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclCardinalTree }
    function GetRoot: IJclCardinalTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclCardinalTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclCardinalTreeIterator = class(TJclAbstractIterator, IJclCardinalIterator, IJclCardinalTreeIterator)
  protected
    FCursor: TJclCardinalTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclCardinalTree;
    FEqualityComparer: IJclCardinalEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclCardinalTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclCardinalTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclCardinalTreeNode; virtual; abstract;
  public
    constructor Create(OwnTree: TJclCardinalTree; ACursor: TJclCardinalTreeNode; AValid: Boolean; AStart: TItrStart);
    { IJclCardinalIterator }
    function Add(AValue: Cardinal): Boolean;
    procedure Extract;
    function GetValue: Cardinal;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(AValue: Cardinal): Boolean;
    function IteratorEquals(const AIterator: IJclCardinalIterator): Boolean;
    function Next: Cardinal;
    function NextIndex: Integer;
    function Previous: Cardinal;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(AValue: Cardinal);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Cardinal read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclCardinalTreeIterator }
    function AddChild(AValue: Cardinal): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): Cardinal;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(AValue: Cardinal): Integer;
    function InsertChild(Index: Integer; AValue: Cardinal): Boolean;
    function Parent: Cardinal;
    procedure SetChild(Index: Integer; AValue: Cardinal);
  end;

  TJclPreOrderCardinalTreeIterator = class(TJclCardinalTreeIterator, IJclCardinalIterator, IJclCardinalTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclCardinalTreeNode; override;
    function GetNextSibling: TJclCardinalTreeNode; override;
    function GetPreviousCursor: TJclCardinalTreeNode; override;
  end;

  TJclPostOrderCardinalTreeIterator = class(TJclCardinalTreeIterator, IJclCardinalIterator, IJclCardinalTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclCardinalTreeNode; override;
    function GetNextSibling: TJclCardinalTreeNode; override;
    function GetPreviousCursor: TJclCardinalTreeNode; override;
  end;

  TJclInt64TreeNode = class
  public
    Value: Int64;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclInt64TreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclInt64TreeNode;
    function IndexOfChild(AChild: TJclInt64TreeNode): Integer;
    function IndexOfValue(const AValue: Int64; const AEqualityComparer: IJclInt64EqualityComparer): Integer;
  end;

  TJclInt64Tree = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclInt64Container, IJclInt64FlatContainer, IJclInt64EqualityComparer,
    IJclInt64Collection, IJclInt64Tree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FRoot: TJclInt64TreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ExtractNode(var ANode: TJclInt64TreeNode);
    procedure RemoveNode(var ANode: TJclInt64TreeNode);
    function CloneNode(Node, Parent: TJclInt64TreeNode): TJclInt64TreeNode;
    function NodeContains(ANode: TJclInt64TreeNode; const AValue: Int64): Boolean;
    procedure PackNode(ANode: TJclInt64TreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create();
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclInt64Collection }
    function Add(const AValue: Int64): Boolean;
    function AddAll(const ACollection: IJclInt64Collection): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclInt64Collection): Boolean;
    function Contains(const AValue: Int64): Boolean;
    function ContainsAll(const ACollection: IJclInt64Collection): Boolean;
    function Extract(const AValue: Int64): Boolean;
    function ExtractAll(const ACollection: IJclInt64Collection): Boolean;
    function First: IJclInt64Iterator;
    function IsEmpty: Boolean;
    function Last: IJclInt64Iterator;
    function Remove(const AValue: Int64): Boolean;
    function RemoveAll(const ACollection: IJclInt64Collection): Boolean;
    function RetainAll(const ACollection: IJclInt64Collection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclInt64Iterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclInt64Tree }
    function GetRoot: IJclInt64TreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclInt64TreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclInt64TreeIterator = class(TJclAbstractIterator, IJclInt64Iterator, IJclInt64TreeIterator)
  protected
    FCursor: TJclInt64TreeNode;
    FStart: TItrStart;
    FOwnTree: TJclInt64Tree;
    FEqualityComparer: IJclInt64EqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclInt64TreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclInt64TreeNode; virtual; abstract;
    function GetPreviousCursor: TJclInt64TreeNode; virtual; abstract;
  public
    constructor Create(OwnTree: TJclInt64Tree; ACursor: TJclInt64TreeNode; AValid: Boolean; AStart: TItrStart);
    { IJclInt64Iterator }
    function Add(const AValue: Int64): Boolean;
    procedure Extract;
    function GetValue: Int64;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Int64): Boolean;
    function IteratorEquals(const AIterator: IJclInt64Iterator): Boolean;
    function Next: Int64;
    function NextIndex: Integer;
    function Previous: Int64;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(const AValue: Int64);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Int64 read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclInt64TreeIterator }
    function AddChild(const AValue: Int64): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): Int64;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AValue: Int64): Integer;
    function InsertChild(Index: Integer; const AValue: Int64): Boolean;
    function Parent: Int64;
    procedure SetChild(Index: Integer; const AValue: Int64);
  end;

  TJclPreOrderInt64TreeIterator = class(TJclInt64TreeIterator, IJclInt64Iterator, IJclInt64TreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclInt64TreeNode; override;
    function GetNextSibling: TJclInt64TreeNode; override;
    function GetPreviousCursor: TJclInt64TreeNode; override;
  end;

  TJclPostOrderInt64TreeIterator = class(TJclInt64TreeIterator, IJclInt64Iterator, IJclInt64TreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclInt64TreeNode; override;
    function GetNextSibling: TJclInt64TreeNode; override;
    function GetPreviousCursor: TJclInt64TreeNode; override;
  end;

  TJclPtrTreeNode = class
  public
    Value: Pointer;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclPtrTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclPtrTreeNode;
    function IndexOfChild(AChild: TJclPtrTreeNode): Integer;
    function IndexOfValue(APtr: Pointer; const AEqualityComparer: IJclPtrEqualityComparer): Integer;
  end;

  TJclPtrTree = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclPtrContainer, IJclPtrFlatContainer, IJclPtrEqualityComparer,
    IJclPtrCollection, IJclPtrTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FRoot: TJclPtrTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ExtractNode(var ANode: TJclPtrTreeNode);
    procedure RemoveNode(var ANode: TJclPtrTreeNode);
    function CloneNode(Node, Parent: TJclPtrTreeNode): TJclPtrTreeNode;
    function NodeContains(ANode: TJclPtrTreeNode; APtr: Pointer): Boolean;
    procedure PackNode(ANode: TJclPtrTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create();
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclPtrCollection }
    function Add(APtr: Pointer): Boolean;
    function AddAll(const ACollection: IJclPtrCollection): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclPtrCollection): Boolean;
    function Contains(APtr: Pointer): Boolean;
    function ContainsAll(const ACollection: IJclPtrCollection): Boolean;
    function Extract(APtr: Pointer): Boolean;
    function ExtractAll(const ACollection: IJclPtrCollection): Boolean;
    function First: IJclPtrIterator;
    function IsEmpty: Boolean;
    function Last: IJclPtrIterator;
    function Remove(APtr: Pointer): Boolean;
    function RemoveAll(const ACollection: IJclPtrCollection): Boolean;
    function RetainAll(const ACollection: IJclPtrCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclPtrIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclPtrTree }
    function GetRoot: IJclPtrTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclPtrTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclPtrTreeIterator = class(TJclAbstractIterator, IJclPtrIterator, IJclPtrTreeIterator)
  protected
    FCursor: TJclPtrTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclPtrTree;
    FEqualityComparer: IJclPtrEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclPtrTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclPtrTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclPtrTreeNode; virtual; abstract;
  public
    constructor Create(OwnTree: TJclPtrTree; ACursor: TJclPtrTreeNode; AValid: Boolean; AStart: TItrStart);
    { IJclPtrIterator }
    function Add(APtr: Pointer): Boolean;
    procedure Extract;
    function GetPointer: Pointer;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(APtr: Pointer): Boolean;
    function IteratorEquals(const AIterator: IJclPtrIterator): Boolean;
    function Next: Pointer;
    function NextIndex: Integer;
    function Previous: Pointer;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetPointer(APtr: Pointer);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Pointer read GetPointer;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclPtrTreeIterator }
    function AddChild(APtr: Pointer): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): Pointer;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(APtr: Pointer): Integer;
    function InsertChild(Index: Integer; APtr: Pointer): Boolean;
    function Parent: Pointer;
    procedure SetChild(Index: Integer; APtr: Pointer);
  end;

  TJclPreOrderPtrTreeIterator = class(TJclPtrTreeIterator, IJclPtrIterator, IJclPtrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclPtrTreeNode; override;
    function GetNextSibling: TJclPtrTreeNode; override;
    function GetPreviousCursor: TJclPtrTreeNode; override;
  end;

  TJclPostOrderPtrTreeIterator = class(TJclPtrTreeIterator, IJclPtrIterator, IJclPtrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclPtrTreeNode; override;
    function GetNextSibling: TJclPtrTreeNode; override;
    function GetPreviousCursor: TJclPtrTreeNode; override;
  end;

  TJclTreeNode = class
  public
    Value: TObject;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclTreeNode;
    function IndexOfChild(AChild: TJclTreeNode): Integer;
    function IndexOfValue(AObject: TObject; const AEqualityComparer: IJclEqualityComparer): Integer;
  end;

  TJclTree = class(TJclAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclContainer, IJclFlatContainer, IJclEqualityComparer, IJclObjectOwner,
    IJclCollection, IJclTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FRoot: TJclTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ExtractNode(var ANode: TJclTreeNode);
    procedure RemoveNode(var ANode: TJclTreeNode);
    function CloneNode(Node, Parent: TJclTreeNode): TJclTreeNode;
    function NodeContains(ANode: TJclTreeNode; AObject: TObject): Boolean;
    procedure PackNode(ANode: TJclTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(AOwnsObjects: Boolean);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCollection }
    function Add(AObject: TObject): Boolean;
    function AddAll(const ACollection: IJclCollection): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclCollection): Boolean;
    function Contains(AObject: TObject): Boolean;
    function ContainsAll(const ACollection: IJclCollection): Boolean;
    function Extract(AObject: TObject): Boolean;
    function ExtractAll(const ACollection: IJclCollection): Boolean;
    function First: IJclIterator;
    function IsEmpty: Boolean;
    function Last: IJclIterator;
    function Remove(AObject: TObject): Boolean;
    function RemoveAll(const ACollection: IJclCollection): Boolean;
    function RetainAll(const ACollection: IJclCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclTree }
    function GetRoot: IJclTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclTreeIterator = class(TJclAbstractIterator, IJclIterator, IJclTreeIterator)
  protected
    FCursor: TJclTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclTree;
    FEqualityComparer: IJclEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclTreeNode; virtual; abstract;
  public
    constructor Create(OwnTree: TJclTree; ACursor: TJclTreeNode; AValid: Boolean; AStart: TItrStart);
    { IJclIterator }
    function Add(AObject: TObject): Boolean;
    procedure Extract;
    function GetObject: TObject;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(AObject: TObject): Boolean;
    function IteratorEquals(const AIterator: IJclIterator): Boolean;
    function Next: TObject;
    function NextIndex: Integer;
    function Previous: TObject;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetObject(AObject: TObject);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: TObject read GetObject;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclTreeIterator }
    function AddChild(AObject: TObject): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): TObject;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(AObject: TObject): Integer;
    function InsertChild(Index: Integer; AObject: TObject): Boolean;
    function Parent: TObject;
    procedure SetChild(Index: Integer; AObject: TObject);
  end;

  TJclPreOrderTreeIterator = class(TJclTreeIterator, IJclIterator, IJclTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclTreeNode; override;
    function GetNextSibling: TJclTreeNode; override;
    function GetPreviousCursor: TJclTreeNode; override;
  end;

  TJclPostOrderTreeIterator = class(TJclTreeIterator, IJclIterator, IJclTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclTreeNode; override;
    function GetNextSibling: TJclTreeNode; override;
    function GetPreviousCursor: TJclTreeNode; override;
  end;


  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN

  TJclTreeNode<T> = class
  public
    Value: T;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclTreeNode<T>;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclTreeNode<T>;
    function IndexOfChild(AChild: TJclTreeNode<T>): Integer;
    function IndexOfValue(const AItem: T; const AEqualityComparer: IJclEqualityComparer<T>): Integer;
  end;

  TJclPreOrderTreeIterator<T> = class;
  TJclPostOrderTreeIterator<T> = class;

  TJclTree<T> = class(TJclAbstractContainer<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclContainer<T>, IJclFlatContainer<T>, IJclEqualityComparer<T>, IJclItemOwner<T>,
    IJclCollection<T>, IJclTree<T>)
  protected
    type
      TTreeNode = TJclTreeNode<T>;
      TPreOrderTreeIterator = TJclPreOrderTreeIterator<T>;
      TPostOrderTreeIterator = TJclPostOrderTreeIterator<T>;
  private
    FRoot: TTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ExtractNode(var ANode: TTreeNode);
    procedure RemoveNode(var ANode: TTreeNode);
    function CloneNode(Node, Parent: TTreeNode): TTreeNode;
    function NodeContains(ANode: TTreeNode; const AItem: T): Boolean;
    procedure PackNode(ANode: TTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(AOwnsItems: Boolean);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCollection<T> }
    function Add(const AItem: T): Boolean;
    function AddAll(const ACollection: IJclCollection<T>): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclCollection<T>): Boolean;
    function Contains(const AItem: T): Boolean;
    function ContainsAll(const ACollection: IJclCollection<T>): Boolean;
    function Extract(const AItem: T): Boolean;
    function ExtractAll(const ACollection: IJclCollection<T>): Boolean;
    function First: IJclIterator<T>;
    function IsEmpty: Boolean;
    function Last: IJclIterator<T>;
    function Remove(const AItem: T): Boolean;
    function RemoveAll(const ACollection: IJclCollection<T>): Boolean;
    function RetainAll(const ACollection: IJclCollection<T>): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIterator<T>;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclTree<T> }
    function GetRoot: IJclTreeIterator<T>;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclTreeIterator<T> read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclTreeIterator<T> = class(TJclAbstractIterator, IJclIterator<T>, IJclTreeIterator<T>)
  protected
    FCursor: TJclTree<T>.TTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclTree<T>;
    FEqualityComparer: IJclEqualityComparer<T>; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclTree<T>.TTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclTree<T>.TTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclTree<T>.TTreeNode; virtual; abstract;
  public
    constructor Create(OwnTree: TJclTree<T>; ACursor: TJclTree<T>.TTreeNode; AValid: Boolean; AStart: TItrStart);
    { IJclIterator<T> }
    function Add(const AItem: T): Boolean;
    procedure Extract;
    function GetItem: T;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AItem: T): Boolean;
    function IteratorEquals(const AIterator: IJclIterator<T>): Boolean;
    function Next: T;
    function NextIndex: Integer;
    function Previous: T;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetItem(const AItem: T);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: T read GetItem;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclTreeIterator<T> }
    function AddChild(const AItem: T): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): T;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AItem: T): Integer;
    function InsertChild(Index: Integer; const AItem: T): Boolean;
    function Parent: T;
    procedure SetChild(Index: Integer; const AItem: T);
  end;

  TJclPreOrderTreeIterator<T> = class(TJclTreeIterator<T>, IJclIterator<T>, IJclTreeIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclTree<T>.TTreeNode; override;
    function GetNextSibling: TJclTree<T>.TTreeNode; override;
    function GetPreviousCursor: TJclTree<T>.TTreeNode; override;
  end;

  TJclPostOrderTreeIterator<T> = class(TJclTreeIterator<T>, IJclIterator<T>, IJclTreeIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclTree<T>.TTreeNode; override;
    function GetNextSibling: TJclTree<T>.TTreeNode; override;
    function GetPreviousCursor: TJclTree<T>.TTreeNode; override;
  end;

  // E = External helper to compare items for equality
  TJclTreeE<T> = class(TJclTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclContainer<T>, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  private
    FEqualityComparer: IJclEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AEqualityComparer: IJclEqualityComparer<T>; AOwnsItems: Boolean);
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    property EqualityComparer: IJclEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclTreeF<T> = class(TJclTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclContainer<T>, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TCompare<T>; AOwnsItems: Boolean);
  end;

  // I = Items can compare themselves to an other for equality
  TJclTreeI<T: IEquatable<T>> = class(TJclTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclContainer<T>, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
  end;

  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

{$IFDEF BCB}
{$IFDEF WIN64}
  {$HPPEMIT '#ifdef MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT ' #undef MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT ' #define JclTrees_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT '#endif'}

  {$HPPEMIT END '#ifdef JclTrees_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END ' #define MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END ' #undef JclTrees_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END '#endif'}
{$ENDIF WIN64}
{$ENDIF BCB}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  System.SysUtils;
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils;
  {$ENDIF ~HAS_UNITSCOPE}

//=== { TJclIntfTreeNode } =======================================================

function TJclIntfTreeNode.IndexOfChild(AChild: TJclIntfTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclIntfTreeNode.IndexOfValue(const AInterface: IInterface;
  const AEqualityComparer: IJclIntfEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclIntfTreeNode(Children[Result]).Value, AInterface) then
      Exit;
  Result := -1;
end;

//=== { TJclIntfTree } =======================================================

constructor TJclIntfTree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclIntfTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclIntfTree.Add(const AInterface: IInterface): Boolean;
var
  NewNode: TJclIntfTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AInterface, nil);

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AInterface)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclIntfTreeNode.Create;
            NewNode.Value := AInterface;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclIntfTreeNode.Create;
        FRoot.Value := AInterface;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTree.AddAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclIntfTree;
  ACollection: IJclIntfCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfTree then
  begin
    ADest := TJclIntfTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclIntfCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclIntfTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfTree then
    TJclIntfTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclIntfTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      RemoveNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTree.CloneNode(Node, Parent: TJclIntfTreeNode): TJclIntfTreeNode;
var
  Index: Integer;
begin
  Result := TJclIntfTreeNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  SetLength(Result.Children, Node.ChildrenCount);
  Result.ChildrenCount := Node.ChildrenCount;
  for Index := 0 to Node.ChildrenCount - 1 do
    Result.Children[Index] := CloneNode(TJclIntfTreeNode(Node.Children[Index]), Result); // recursive call
end;

function TJclIntfTree.CollectionEquals(const ACollection: IJclIntfCollection): Boolean;
var
  It, ItSelf: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTree.Contains(const AInterface: IInterface): Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AInterface)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTree.ContainsAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTree.Extract(const AInterface: IInterface): Boolean;
var
  It: IJclIntfIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AInterface) then
      begin
        It.Extract;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTree.ExtractAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfTree.ExtractNode(var ANode: TJclIntfTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclIntfTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ExtractNode(TJclIntfTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ExtractNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclIntfTree.First: IJclIntfIterator;
var
  Start: TJclIntfTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderIntfTreeIterator.Create(Self, Start, False, isFirst);
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclIntfTreeNode(Start.Children[0]);
          Result := TJclPostOrderIntfTreeIterator.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntfTree.GetEnumerator: IJclIntfIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntfTree.GetRoot: IJclIntfTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderIntfTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderIntfTreeIterator.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclIntfTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfTree.Last: IJclIntfIterator;
var
  Start: TJclIntfTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while Start.ChildrenCount > 0 do
              Start := TJclIntfTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TJclPreOrderIntfTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderIntfTreeIterator.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTree.NodeContains(ANode: TJclIntfTreeNode; const AInterface: IInterface): Boolean;
var
  Index: Integer;
begin
  Result := ItemsEqual(ANode.Value, AInterface);
  if not Result then
    for Index := 0 to ANode.ChildrenCount - 1 do
  begin
    Result := NodeContains(TJclIntfTreeNode(ANode.Children[Index]), AInterface);
    if Result then
      Break;
  end;
end;

procedure TJclIntfTree.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfTree.PackNode(ANode: TJclIntfTreeNode);
var
  Index: Integer;
begin
  SetLength(ANode.Children, ANode.ChildrenCount);
  for Index := 0 to ANode.ChildrenCount - 1 do
    PackNode(TJclIntfTreeNode(ANode.Children[Index]));
end;

function TJclIntfTree.Remove(const AInterface: IInterface): Boolean;
var
  Extracted: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AInterface);
    if Result then
    begin
      Extracted := AInterface;
      FreeObject(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTree.RemoveAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfTree.RemoveNode(var ANode: TJclIntfTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclIntfTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    RemoveNode(TJclIntfTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    RemoveNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeObject(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclIntfTree.RetainAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntfTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclIntfTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfTree.Create;
  AssignPropertiesTo(Result);
end;

//=== { TJclIntfTreeIterator } ===========================================================

constructor TJclIntfTreeIterator.Create(OwnTree: TJclIntfTree; ACursor: TJclIntfTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclIntfEqualityComparer;
end;

function TJclIntfTreeIterator.Add(const AInterface: IInterface): Boolean;
var
  ParentNode, NewNode: TJclIntfTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclIntfTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AInterface, nil))
      and ((not FOwnTree.Contains(AInterface)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclIntfTreeNode.Create;
        NewNode.Value := AInterface;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTreeIterator.AddChild(const AInterface: IInterface): Boolean;
var
  NewNode: TJclIntfTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AInterface, nil))
      and ((not FOwnTree.Contains(AInterface)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclIntfTreeNode.Create;
        NewNode.Value := AInterface;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclIntfTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclIntfTreeIterator then
  begin
    ADest := TJclIntfTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclIntfTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfTreeIterator.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.RemoveNode(TJclIntfTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.RemoveNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfTreeIterator.DeleteChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.RemoveNode(TJclIntfTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.RemoveNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfTreeIterator.Extract;
var
  OldCursor: TJclIntfTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ExtractNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfTreeIterator.ExtractChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ExtractNode(TJclIntfTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ExtractNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfTreeIterator.ExtractChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ExtractNode(TJclIntfTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ExtractNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTreeIterator.GetChild(Index: Integer): IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclIntfTreeNode(FCursor.Children[Index]);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTreeIterator.GetObject: IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTreeIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTreeIterator.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTreeIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTreeIterator.IndexOfChild(const AInterface: IInterface): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AInterface, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTreeIterator.Insert(const AInterface: IInterface): Boolean;
var
  ParentNode, NewNode: TJclIntfTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclIntfTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AInterface, nil))
      and ((not FOwnTree.Contains(AInterface)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclIntfTreeNode.Create;
        NewNode.Value := AInterface;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTreeIterator.InsertChild(Index: Integer; const AInterface: IInterface): Boolean;
var
  NewNode: TJclIntfTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclIntfTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AInterface, nil))
      and ((not FOwnTree.Contains(AInterface)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclIntfTreeNode.Create;
        NewNode.Value := AInterface;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTreeIterator.IteratorEquals(const AIterator: IJclIntfIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclIntfTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclIntfTreeIterator then
  begin
    ItrObj := TJclIntfTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntfTreeIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntfTreeIterator.Next: IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclIntfTreeIterator.Parent: IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTreeIterator.Previous: IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntfTreeIterator.Remove;
var
  OldCursor: TJclIntfTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.RemoveNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfTreeIterator.Reset;
var
  NewCursor: TJclIntfTreeNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfTreeIterator.SetChild(Index: Integer; const AInterface: IInterface);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclIntfTreeNode(FCursor.Children[Index]).Value := AInterface
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfTreeIterator.SetObject(const AInterface: IInterface);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeObject(FCursor.Value);
      FCursor.Value := AInterface;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclPreOrderIntfTreeIterator } ===================================================

function TJclPreOrderIntfTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderIntfTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderIntfTreeIterator.GetNextCursor: TJclIntfTreeNode;
var
  LastRet: TJclIntfTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclIntfTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclIntfTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TJclPreOrderIntfTreeIterator.GetNextSibling: TJclIntfTreeNode;
var
  LastRet: TJclIntfTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclIntfTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TJclPreOrderIntfTreeIterator.GetPreviousCursor: TJclIntfTreeNode;
var
  LastRet: TJclIntfTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclIntfTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclIntfTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TJclPostOrderIntfTreeIterator } ==================================================

function TJclPostOrderIntfTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderIntfTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderIntfTreeIterator.GetNextCursor: TJclIntfTreeNode;
var
  LastRet: TJclIntfTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclIntfTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclIntfTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderIntfTreeIterator.GetNextSibling: TJclIntfTreeNode;
var
  LastRet: TJclIntfTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclIntfTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclIntfTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderIntfTreeIterator.GetPreviousCursor: TJclIntfTreeNode;
var
  LastRet: TJclIntfTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclIntfTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclIntfTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

//=== { TJclAnsiStrTreeNode } =======================================================

function TJclAnsiStrTreeNode.IndexOfChild(AChild: TJclAnsiStrTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclAnsiStrTreeNode.IndexOfValue(const AString: AnsiString;
  const AEqualityComparer: IJclAnsiStrEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclAnsiStrTreeNode(Children[Result]).Value, AString) then
      Exit;
  Result := -1;
end;

//=== { TJclAnsiStrTree } =======================================================

constructor TJclAnsiStrTree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclAnsiStrTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclAnsiStrTree.Add(const AString: AnsiString): Boolean;
var
  NewNode: TJclAnsiStrTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AString, '');

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AString)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclAnsiStrTreeNode.Create;
            NewNode.Value := AString;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclAnsiStrTreeNode.Create;
        FRoot.Value := AString;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTree.AddAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclAnsiStrTree;
  ACollection: IJclAnsiStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrTree then
  begin
    ADest := TJclAnsiStrTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclAnsiStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclAnsiStrTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclAnsiStrTree then
    TJclAnsiStrTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclAnsiStrTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      RemoveNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTree.CloneNode(Node, Parent: TJclAnsiStrTreeNode): TJclAnsiStrTreeNode;
var
  Index: Integer;
begin
  Result := TJclAnsiStrTreeNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  SetLength(Result.Children, Node.ChildrenCount);
  Result.ChildrenCount := Node.ChildrenCount;
  for Index := 0 to Node.ChildrenCount - 1 do
    Result.Children[Index] := CloneNode(TJclAnsiStrTreeNode(Node.Children[Index]), Result); // recursive call
end;

function TJclAnsiStrTree.CollectionEquals(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It, ItSelf: IJclAnsiStrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTree.Contains(const AString: AnsiString): Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AString)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTree.ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTree.Extract(const AString: AnsiString): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AString) then
      begin
        It.Extract;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTree.ExtractAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrTree.ExtractNode(var ANode: TJclAnsiStrTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclAnsiStrTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ExtractNode(TJclAnsiStrTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ExtractNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclAnsiStrTree.First: IJclAnsiStrIterator;
var
  Start: TJclAnsiStrTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderAnsiStrTreeIterator.Create(Self, Start, False, isFirst);
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclAnsiStrTreeNode(Start.Children[0]);
          Result := TJclPostOrderAnsiStrTreeIterator.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclAnsiStrTree.GetEnumerator: IJclAnsiStrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclAnsiStrTree.GetRoot: IJclAnsiStrTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderAnsiStrTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderAnsiStrTreeIterator.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclAnsiStrTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrTree.Last: IJclAnsiStrIterator;
var
  Start: TJclAnsiStrTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while Start.ChildrenCount > 0 do
              Start := TJclAnsiStrTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TJclPreOrderAnsiStrTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderAnsiStrTreeIterator.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTree.NodeContains(ANode: TJclAnsiStrTreeNode; const AString: AnsiString): Boolean;
var
  Index: Integer;
begin
  Result := ItemsEqual(ANode.Value, AString);
  if not Result then
    for Index := 0 to ANode.ChildrenCount - 1 do
  begin
    Result := NodeContains(TJclAnsiStrTreeNode(ANode.Children[Index]), AString);
    if Result then
      Break;
  end;
end;

procedure TJclAnsiStrTree.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrTree.PackNode(ANode: TJclAnsiStrTreeNode);
var
  Index: Integer;
begin
  SetLength(ANode.Children, ANode.ChildrenCount);
  for Index := 0 to ANode.ChildrenCount - 1 do
    PackNode(TJclAnsiStrTreeNode(ANode.Children[Index]));
end;

function TJclAnsiStrTree.Remove(const AString: AnsiString): Boolean;
var
  Extracted: AnsiString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AString);
    if Result then
    begin
      Extracted := AString;
      FreeString(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTree.RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrTree.RemoveNode(var ANode: TJclAnsiStrTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclAnsiStrTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    RemoveNode(TJclAnsiStrTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    RemoveNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeString(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclAnsiStrTree.RetainAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclAnsiStrTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclAnsiStrTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrTree.Create;
  AssignPropertiesTo(Result);
end;

//=== { TJclAnsiStrTreeIterator } ===========================================================

constructor TJclAnsiStrTreeIterator.Create(OwnTree: TJclAnsiStrTree; ACursor: TJclAnsiStrTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclAnsiStrEqualityComparer;
end;

function TJclAnsiStrTreeIterator.Add(const AString: AnsiString): Boolean;
var
  ParentNode, NewNode: TJclAnsiStrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclAnsiStrTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclAnsiStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTreeIterator.AddChild(const AString: AnsiString): Boolean;
var
  NewNode: TJclAnsiStrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclAnsiStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclAnsiStrTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclAnsiStrTreeIterator then
  begin
    ADest := TJclAnsiStrTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclAnsiStrTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrTreeIterator.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.RemoveNode(TJclAnsiStrTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.RemoveNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrTreeIterator.DeleteChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.RemoveNode(TJclAnsiStrTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.RemoveNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrTreeIterator.Extract;
var
  OldCursor: TJclAnsiStrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ExtractNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrTreeIterator.ExtractChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ExtractNode(TJclAnsiStrTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ExtractNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrTreeIterator.ExtractChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ExtractNode(TJclAnsiStrTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ExtractNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTreeIterator.GetChild(Index: Integer): AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclAnsiStrTreeNode(FCursor.Children[Index]);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTreeIterator.GetString: AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTreeIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTreeIterator.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTreeIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTreeIterator.IndexOfChild(const AString: AnsiString): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AString, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTreeIterator.Insert(const AString: AnsiString): Boolean;
var
  ParentNode, NewNode: TJclAnsiStrTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclAnsiStrTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclAnsiStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTreeIterator.InsertChild(Index: Integer; const AString: AnsiString): Boolean;
var
  NewNode: TJclAnsiStrTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclAnsiStrTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclAnsiStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTreeIterator.IteratorEquals(const AIterator: IJclAnsiStrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclAnsiStrTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclAnsiStrTreeIterator then
  begin
    ItrObj := TJclAnsiStrTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclAnsiStrTreeIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclAnsiStrTreeIterator.Next: AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclAnsiStrTreeIterator.Parent: AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTreeIterator.Previous: AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclAnsiStrTreeIterator.Remove;
var
  OldCursor: TJclAnsiStrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.RemoveNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrTreeIterator.Reset;
var
  NewCursor: TJclAnsiStrTreeNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrTreeIterator.SetChild(Index: Integer; const AString: AnsiString);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclAnsiStrTreeNode(FCursor.Children[Index]).Value := AString
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrTreeIterator.SetString(const AString: AnsiString);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeString(FCursor.Value);
      FCursor.Value := AString;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclPreOrderAnsiStrTreeIterator } ===================================================

function TJclPreOrderAnsiStrTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderAnsiStrTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderAnsiStrTreeIterator.GetNextCursor: TJclAnsiStrTreeNode;
var
  LastRet: TJclAnsiStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclAnsiStrTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclAnsiStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TJclPreOrderAnsiStrTreeIterator.GetNextSibling: TJclAnsiStrTreeNode;
var
  LastRet: TJclAnsiStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclAnsiStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TJclPreOrderAnsiStrTreeIterator.GetPreviousCursor: TJclAnsiStrTreeNode;
var
  LastRet: TJclAnsiStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclAnsiStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclAnsiStrTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TJclPostOrderAnsiStrTreeIterator } ==================================================

function TJclPostOrderAnsiStrTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderAnsiStrTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderAnsiStrTreeIterator.GetNextCursor: TJclAnsiStrTreeNode;
var
  LastRet: TJclAnsiStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclAnsiStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclAnsiStrTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderAnsiStrTreeIterator.GetNextSibling: TJclAnsiStrTreeNode;
var
  LastRet: TJclAnsiStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclAnsiStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclAnsiStrTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderAnsiStrTreeIterator.GetPreviousCursor: TJclAnsiStrTreeNode;
var
  LastRet: TJclAnsiStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclAnsiStrTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclAnsiStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

//=== { TJclWideStrTreeNode } =======================================================

function TJclWideStrTreeNode.IndexOfChild(AChild: TJclWideStrTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclWideStrTreeNode.IndexOfValue(const AString: WideString;
  const AEqualityComparer: IJclWideStrEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclWideStrTreeNode(Children[Result]).Value, AString) then
      Exit;
  Result := -1;
end;

//=== { TJclWideStrTree } =======================================================

constructor TJclWideStrTree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclWideStrTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclWideStrTree.Add(const AString: WideString): Boolean;
var
  NewNode: TJclWideStrTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AString, '');

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AString)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclWideStrTreeNode.Create;
            NewNode.Value := AString;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclWideStrTreeNode.Create;
        FRoot.Value := AString;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTree.AddAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclWideStrTree;
  ACollection: IJclWideStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrTree then
  begin
    ADest := TJclWideStrTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclWideStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclWideStrTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclWideStrTree then
    TJclWideStrTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclWideStrTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      RemoveNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTree.CloneNode(Node, Parent: TJclWideStrTreeNode): TJclWideStrTreeNode;
var
  Index: Integer;
begin
  Result := TJclWideStrTreeNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  SetLength(Result.Children, Node.ChildrenCount);
  Result.ChildrenCount := Node.ChildrenCount;
  for Index := 0 to Node.ChildrenCount - 1 do
    Result.Children[Index] := CloneNode(TJclWideStrTreeNode(Node.Children[Index]), Result); // recursive call
end;

function TJclWideStrTree.CollectionEquals(const ACollection: IJclWideStrCollection): Boolean;
var
  It, ItSelf: IJclWideStrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTree.Contains(const AString: WideString): Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AString)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTree.ContainsAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTree.Extract(const AString: WideString): Boolean;
var
  It: IJclWideStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AString) then
      begin
        It.Extract;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTree.ExtractAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrTree.ExtractNode(var ANode: TJclWideStrTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclWideStrTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ExtractNode(TJclWideStrTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ExtractNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclWideStrTree.First: IJclWideStrIterator;
var
  Start: TJclWideStrTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderWideStrTreeIterator.Create(Self, Start, False, isFirst);
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclWideStrTreeNode(Start.Children[0]);
          Result := TJclPostOrderWideStrTreeIterator.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclWideStrTree.GetEnumerator: IJclWideStrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclWideStrTree.GetRoot: IJclWideStrTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderWideStrTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderWideStrTreeIterator.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclWideStrTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrTree.Last: IJclWideStrIterator;
var
  Start: TJclWideStrTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while Start.ChildrenCount > 0 do
              Start := TJclWideStrTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TJclPreOrderWideStrTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderWideStrTreeIterator.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTree.NodeContains(ANode: TJclWideStrTreeNode; const AString: WideString): Boolean;
var
  Index: Integer;
begin
  Result := ItemsEqual(ANode.Value, AString);
  if not Result then
    for Index := 0 to ANode.ChildrenCount - 1 do
  begin
    Result := NodeContains(TJclWideStrTreeNode(ANode.Children[Index]), AString);
    if Result then
      Break;
  end;
end;

procedure TJclWideStrTree.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrTree.PackNode(ANode: TJclWideStrTreeNode);
var
  Index: Integer;
begin
  SetLength(ANode.Children, ANode.ChildrenCount);
  for Index := 0 to ANode.ChildrenCount - 1 do
    PackNode(TJclWideStrTreeNode(ANode.Children[Index]));
end;

function TJclWideStrTree.Remove(const AString: WideString): Boolean;
var
  Extracted: WideString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AString);
    if Result then
    begin
      Extracted := AString;
      FreeString(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTree.RemoveAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrTree.RemoveNode(var ANode: TJclWideStrTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclWideStrTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    RemoveNode(TJclWideStrTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    RemoveNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeString(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclWideStrTree.RetainAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclWideStrTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclWideStrTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrTree.Create;
  AssignPropertiesTo(Result);
end;

//=== { TJclWideStrTreeIterator } ===========================================================

constructor TJclWideStrTreeIterator.Create(OwnTree: TJclWideStrTree; ACursor: TJclWideStrTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclWideStrEqualityComparer;
end;

function TJclWideStrTreeIterator.Add(const AString: WideString): Boolean;
var
  ParentNode, NewNode: TJclWideStrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclWideStrTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclWideStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTreeIterator.AddChild(const AString: WideString): Boolean;
var
  NewNode: TJclWideStrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclWideStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclWideStrTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclWideStrTreeIterator then
  begin
    ADest := TJclWideStrTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclWideStrTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrTreeIterator.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.RemoveNode(TJclWideStrTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.RemoveNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrTreeIterator.DeleteChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.RemoveNode(TJclWideStrTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.RemoveNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrTreeIterator.Extract;
var
  OldCursor: TJclWideStrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ExtractNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrTreeIterator.ExtractChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ExtractNode(TJclWideStrTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ExtractNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrTreeIterator.ExtractChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ExtractNode(TJclWideStrTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ExtractNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTreeIterator.GetChild(Index: Integer): WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclWideStrTreeNode(FCursor.Children[Index]);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTreeIterator.GetString: WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTreeIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTreeIterator.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTreeIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTreeIterator.IndexOfChild(const AString: WideString): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AString, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTreeIterator.Insert(const AString: WideString): Boolean;
var
  ParentNode, NewNode: TJclWideStrTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclWideStrTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclWideStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTreeIterator.InsertChild(Index: Integer; const AString: WideString): Boolean;
var
  NewNode: TJclWideStrTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclWideStrTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclWideStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTreeIterator.IteratorEquals(const AIterator: IJclWideStrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclWideStrTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclWideStrTreeIterator then
  begin
    ItrObj := TJclWideStrTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclWideStrTreeIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclWideStrTreeIterator.Next: WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclWideStrTreeIterator.Parent: WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTreeIterator.Previous: WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclWideStrTreeIterator.Remove;
var
  OldCursor: TJclWideStrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.RemoveNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrTreeIterator.Reset;
var
  NewCursor: TJclWideStrTreeNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrTreeIterator.SetChild(Index: Integer; const AString: WideString);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclWideStrTreeNode(FCursor.Children[Index]).Value := AString
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrTreeIterator.SetString(const AString: WideString);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeString(FCursor.Value);
      FCursor.Value := AString;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclPreOrderWideStrTreeIterator } ===================================================

function TJclPreOrderWideStrTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderWideStrTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderWideStrTreeIterator.GetNextCursor: TJclWideStrTreeNode;
var
  LastRet: TJclWideStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclWideStrTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclWideStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TJclPreOrderWideStrTreeIterator.GetNextSibling: TJclWideStrTreeNode;
var
  LastRet: TJclWideStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclWideStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TJclPreOrderWideStrTreeIterator.GetPreviousCursor: TJclWideStrTreeNode;
var
  LastRet: TJclWideStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclWideStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclWideStrTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TJclPostOrderWideStrTreeIterator } ==================================================

function TJclPostOrderWideStrTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderWideStrTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderWideStrTreeIterator.GetNextCursor: TJclWideStrTreeNode;
var
  LastRet: TJclWideStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclWideStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclWideStrTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderWideStrTreeIterator.GetNextSibling: TJclWideStrTreeNode;
var
  LastRet: TJclWideStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclWideStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclWideStrTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderWideStrTreeIterator.GetPreviousCursor: TJclWideStrTreeNode;
var
  LastRet: TJclWideStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclWideStrTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclWideStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrTreeNode } =======================================================

function TJclUnicodeStrTreeNode.IndexOfChild(AChild: TJclUnicodeStrTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclUnicodeStrTreeNode.IndexOfValue(const AString: UnicodeString;
  const AEqualityComparer: IJclUnicodeStrEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclUnicodeStrTreeNode(Children[Result]).Value, AString) then
      Exit;
  Result := -1;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrTree } =======================================================

constructor TJclUnicodeStrTree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclUnicodeStrTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclUnicodeStrTree.Add(const AString: UnicodeString): Boolean;
var
  NewNode: TJclUnicodeStrTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AString, '');

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AString)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclUnicodeStrTreeNode.Create;
            NewNode.Value := AString;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclUnicodeStrTreeNode.Create;
        FRoot.Value := AString;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTree.AddAll(const ACollection: IJclUnicodeStrCollection): Boolean;
var
  It: IJclUnicodeStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclUnicodeStrTree;
  ACollection: IJclUnicodeStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclUnicodeStrTree then
  begin
    ADest := TJclUnicodeStrTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclUnicodeStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclUnicodeStrTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclUnicodeStrTree then
    TJclUnicodeStrTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclUnicodeStrTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      RemoveNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTree.CloneNode(Node, Parent: TJclUnicodeStrTreeNode): TJclUnicodeStrTreeNode;
var
  Index: Integer;
begin
  Result := TJclUnicodeStrTreeNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  SetLength(Result.Children, Node.ChildrenCount);
  Result.ChildrenCount := Node.ChildrenCount;
  for Index := 0 to Node.ChildrenCount - 1 do
    Result.Children[Index] := CloneNode(TJclUnicodeStrTreeNode(Node.Children[Index]), Result); // recursive call
end;

function TJclUnicodeStrTree.CollectionEquals(const ACollection: IJclUnicodeStrCollection): Boolean;
var
  It, ItSelf: IJclUnicodeStrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTree.Contains(const AString: UnicodeString): Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AString)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTree.ContainsAll(const ACollection: IJclUnicodeStrCollection): Boolean;
var
  It: IJclUnicodeStrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTree.Extract(const AString: UnicodeString): Boolean;
var
  It: IJclUnicodeStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AString) then
      begin
        It.Extract;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTree.ExtractAll(const ACollection: IJclUnicodeStrCollection): Boolean;
var
  It: IJclUnicodeStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrTree.ExtractNode(var ANode: TJclUnicodeStrTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclUnicodeStrTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ExtractNode(TJclUnicodeStrTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ExtractNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclUnicodeStrTree.First: IJclUnicodeStrIterator;
var
  Start: TJclUnicodeStrTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderUnicodeStrTreeIterator.Create(Self, Start, False, isFirst);
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclUnicodeStrTreeNode(Start.Children[0]);
          Result := TJclPostOrderUnicodeStrTreeIterator.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclUnicodeStrTree.GetEnumerator: IJclUnicodeStrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclUnicodeStrTree.GetRoot: IJclUnicodeStrTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderUnicodeStrTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderUnicodeStrTreeIterator.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclUnicodeStrTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclUnicodeStrTree.Last: IJclUnicodeStrIterator;
var
  Start: TJclUnicodeStrTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while Start.ChildrenCount > 0 do
              Start := TJclUnicodeStrTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TJclPreOrderUnicodeStrTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderUnicodeStrTreeIterator.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTree.NodeContains(ANode: TJclUnicodeStrTreeNode; const AString: UnicodeString): Boolean;
var
  Index: Integer;
begin
  Result := ItemsEqual(ANode.Value, AString);
  if not Result then
    for Index := 0 to ANode.ChildrenCount - 1 do
  begin
    Result := NodeContains(TJclUnicodeStrTreeNode(ANode.Children[Index]), AString);
    if Result then
      Break;
  end;
end;

procedure TJclUnicodeStrTree.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrTree.PackNode(ANode: TJclUnicodeStrTreeNode);
var
  Index: Integer;
begin
  SetLength(ANode.Children, ANode.ChildrenCount);
  for Index := 0 to ANode.ChildrenCount - 1 do
    PackNode(TJclUnicodeStrTreeNode(ANode.Children[Index]));
end;

function TJclUnicodeStrTree.Remove(const AString: UnicodeString): Boolean;
var
  Extracted: UnicodeString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AString);
    if Result then
    begin
      Extracted := AString;
      FreeString(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTree.RemoveAll(const ACollection: IJclUnicodeStrCollection): Boolean;
var
  It: IJclUnicodeStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrTree.RemoveNode(var ANode: TJclUnicodeStrTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclUnicodeStrTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    RemoveNode(TJclUnicodeStrTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    RemoveNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeString(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclUnicodeStrTree.RetainAll(const ACollection: IJclUnicodeStrCollection): Boolean;
var
  It: IJclUnicodeStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclUnicodeStrTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclUnicodeStrTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclUnicodeStrTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrTree.Create;
  AssignPropertiesTo(Result);
end;

{$ENDIF SUPPORTS_UNICODE_STRING}

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrTreeIterator } ===========================================================

constructor TJclUnicodeStrTreeIterator.Create(OwnTree: TJclUnicodeStrTree; ACursor: TJclUnicodeStrTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclUnicodeStrEqualityComparer;
end;

function TJclUnicodeStrTreeIterator.Add(const AString: UnicodeString): Boolean;
var
  ParentNode, NewNode: TJclUnicodeStrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclUnicodeStrTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclUnicodeStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTreeIterator.AddChild(const AString: UnicodeString): Boolean;
var
  NewNode: TJclUnicodeStrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclUnicodeStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclUnicodeStrTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclUnicodeStrTreeIterator then
  begin
    ADest := TJclUnicodeStrTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclUnicodeStrTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrTreeIterator.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.RemoveNode(TJclUnicodeStrTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.RemoveNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrTreeIterator.DeleteChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.RemoveNode(TJclUnicodeStrTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.RemoveNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrTreeIterator.Extract;
var
  OldCursor: TJclUnicodeStrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ExtractNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrTreeIterator.ExtractChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ExtractNode(TJclUnicodeStrTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ExtractNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrTreeIterator.ExtractChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ExtractNode(TJclUnicodeStrTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ExtractNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTreeIterator.GetChild(Index: Integer): UnicodeString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclUnicodeStrTreeNode(FCursor.Children[Index]);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTreeIterator.GetString: UnicodeString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTreeIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTreeIterator.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTreeIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTreeIterator.IndexOfChild(const AString: UnicodeString): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AString, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTreeIterator.Insert(const AString: UnicodeString): Boolean;
var
  ParentNode, NewNode: TJclUnicodeStrTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclUnicodeStrTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclUnicodeStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTreeIterator.InsertChild(Index: Integer; const AString: UnicodeString): Boolean;
var
  NewNode: TJclUnicodeStrTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclUnicodeStrTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclUnicodeStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTreeIterator.IteratorEquals(const AIterator: IJclUnicodeStrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclUnicodeStrTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclUnicodeStrTreeIterator then
  begin
    ItrObj := TJclUnicodeStrTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclUnicodeStrTreeIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclUnicodeStrTreeIterator.Next: UnicodeString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclUnicodeStrTreeIterator.Parent: UnicodeString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTreeIterator.Previous: UnicodeString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclUnicodeStrTreeIterator.Remove;
var
  OldCursor: TJclUnicodeStrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.RemoveNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrTreeIterator.Reset;
var
  NewCursor: TJclUnicodeStrTreeNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrTreeIterator.SetChild(Index: Integer; const AString: UnicodeString);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclUnicodeStrTreeNode(FCursor.Children[Index]).Value := AString
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrTreeIterator.SetString(const AString: UnicodeString);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeString(FCursor.Value);
      FCursor.Value := AString;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclPreOrderUnicodeStrTreeIterator } ===================================================

function TJclPreOrderUnicodeStrTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderUnicodeStrTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderUnicodeStrTreeIterator.GetNextCursor: TJclUnicodeStrTreeNode;
var
  LastRet: TJclUnicodeStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclUnicodeStrTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclUnicodeStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TJclPreOrderUnicodeStrTreeIterator.GetNextSibling: TJclUnicodeStrTreeNode;
var
  LastRet: TJclUnicodeStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclUnicodeStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TJclPreOrderUnicodeStrTreeIterator.GetPreviousCursor: TJclUnicodeStrTreeNode;
var
  LastRet: TJclUnicodeStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclUnicodeStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclUnicodeStrTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TJclPostOrderUnicodeStrTreeIterator } ==================================================

function TJclPostOrderUnicodeStrTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderUnicodeStrTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderUnicodeStrTreeIterator.GetNextCursor: TJclUnicodeStrTreeNode;
var
  LastRet: TJclUnicodeStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclUnicodeStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclUnicodeStrTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderUnicodeStrTreeIterator.GetNextSibling: TJclUnicodeStrTreeNode;
var
  LastRet: TJclUnicodeStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclUnicodeStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclUnicodeStrTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderUnicodeStrTreeIterator.GetPreviousCursor: TJclUnicodeStrTreeNode;
var
  LastRet: TJclUnicodeStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclUnicodeStrTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclUnicodeStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

//=== { TJclSingleTreeNode } =======================================================

function TJclSingleTreeNode.IndexOfChild(AChild: TJclSingleTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclSingleTreeNode.IndexOfValue(const AValue: Single;
  const AEqualityComparer: IJclSingleEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclSingleTreeNode(Children[Result]).Value, AValue) then
      Exit;
  Result := -1;
end;

//=== { TJclSingleTree } =======================================================

constructor TJclSingleTree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclSingleTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclSingleTree.Add(const AValue: Single): Boolean;
var
  NewNode: TJclSingleTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AValue, 0.0);

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AValue)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclSingleTreeNode.Create;
            NewNode.Value := AValue;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclSingleTreeNode.Create;
        FRoot.Value := AValue;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTree.AddAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclSingleTree;
  ACollection: IJclSingleCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclSingleTree then
  begin
    ADest := TJclSingleTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclSingleCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclSingleTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclSingleTree then
    TJclSingleTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclSingleTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      RemoveNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTree.CloneNode(Node, Parent: TJclSingleTreeNode): TJclSingleTreeNode;
var
  Index: Integer;
begin
  Result := TJclSingleTreeNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  SetLength(Result.Children, Node.ChildrenCount);
  Result.ChildrenCount := Node.ChildrenCount;
  for Index := 0 to Node.ChildrenCount - 1 do
    Result.Children[Index] := CloneNode(TJclSingleTreeNode(Node.Children[Index]), Result); // recursive call
end;

function TJclSingleTree.CollectionEquals(const ACollection: IJclSingleCollection): Boolean;
var
  It, ItSelf: IJclSingleIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTree.Contains(const AValue: Single): Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AValue)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTree.ContainsAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTree.Extract(const AValue: Single): Boolean;
var
  It: IJclSingleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AValue) then
      begin
        It.Extract;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTree.ExtractAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleTree.ExtractNode(var ANode: TJclSingleTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclSingleTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ExtractNode(TJclSingleTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ExtractNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclSingleTree.First: IJclSingleIterator;
var
  Start: TJclSingleTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderSingleTreeIterator.Create(Self, Start, False, isFirst);
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclSingleTreeNode(Start.Children[0]);
          Result := TJclPostOrderSingleTreeIterator.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclSingleTree.GetEnumerator: IJclSingleIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclSingleTree.GetRoot: IJclSingleTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderSingleTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderSingleTreeIterator.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclSingleTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleTree.Last: IJclSingleIterator;
var
  Start: TJclSingleTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while Start.ChildrenCount > 0 do
              Start := TJclSingleTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TJclPreOrderSingleTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderSingleTreeIterator.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTree.NodeContains(ANode: TJclSingleTreeNode; const AValue: Single): Boolean;
var
  Index: Integer;
begin
  Result := ItemsEqual(ANode.Value, AValue);
  if not Result then
    for Index := 0 to ANode.ChildrenCount - 1 do
  begin
    Result := NodeContains(TJclSingleTreeNode(ANode.Children[Index]), AValue);
    if Result then
      Break;
  end;
end;

procedure TJclSingleTree.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleTree.PackNode(ANode: TJclSingleTreeNode);
var
  Index: Integer;
begin
  SetLength(ANode.Children, ANode.ChildrenCount);
  for Index := 0 to ANode.ChildrenCount - 1 do
    PackNode(TJclSingleTreeNode(ANode.Children[Index]));
end;

function TJclSingleTree.Remove(const AValue: Single): Boolean;
var
  Extracted: Single;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AValue);
    if Result then
    begin
      Extracted := AValue;
      FreeSingle(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTree.RemoveAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleTree.RemoveNode(var ANode: TJclSingleTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclSingleTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    RemoveNode(TJclSingleTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    RemoveNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeSingle(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclSingleTree.RetainAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclSingleTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclSingleTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclSingleTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleTree.Create;
  AssignPropertiesTo(Result);
end;

//=== { TJclSingleTreeIterator } ===========================================================

constructor TJclSingleTreeIterator.Create(OwnTree: TJclSingleTree; ACursor: TJclSingleTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclSingleEqualityComparer;
end;

function TJclSingleTreeIterator.Add(const AValue: Single): Boolean;
var
  ParentNode, NewNode: TJclSingleTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclSingleTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclSingleTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTreeIterator.AddChild(const AValue: Single): Boolean;
var
  NewNode: TJclSingleTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclSingleTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclSingleTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclSingleTreeIterator then
  begin
    ADest := TJclSingleTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclSingleTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleTreeIterator.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.RemoveNode(TJclSingleTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.RemoveNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleTreeIterator.DeleteChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.RemoveNode(TJclSingleTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.RemoveNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleTreeIterator.Extract;
var
  OldCursor: TJclSingleTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ExtractNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleTreeIterator.ExtractChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ExtractNode(TJclSingleTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ExtractNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleTreeIterator.ExtractChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ExtractNode(TJclSingleTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ExtractNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTreeIterator.GetChild(Index: Integer): Single;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclSingleTreeNode(FCursor.Children[Index]);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTreeIterator.GetValue: Single;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := 0.0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTreeIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTreeIterator.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTreeIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTreeIterator.IndexOfChild(const AValue: Single): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AValue, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTreeIterator.Insert(const AValue: Single): Boolean;
var
  ParentNode, NewNode: TJclSingleTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclSingleTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclSingleTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTreeIterator.InsertChild(Index: Integer; const AValue: Single): Boolean;
var
  NewNode: TJclSingleTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclSingleTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclSingleTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTreeIterator.IteratorEquals(const AIterator: IJclSingleIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclSingleTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclSingleTreeIterator then
  begin
    ItrObj := TJclSingleTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclSingleTreeIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclSingleTreeIterator.Next: Single;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := 0.0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclSingleTreeIterator.Parent: Single;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTreeIterator.Previous: Single;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := 0.0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclSingleTreeIterator.Remove;
var
  OldCursor: TJclSingleTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.RemoveNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleTreeIterator.Reset;
var
  NewCursor: TJclSingleTreeNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleTreeIterator.SetChild(Index: Integer; const AValue: Single);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclSingleTreeNode(FCursor.Children[Index]).Value := AValue
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleTreeIterator.SetValue(const AValue: Single);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeSingle(FCursor.Value);
      FCursor.Value := AValue;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclPreOrderSingleTreeIterator } ===================================================

function TJclPreOrderSingleTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderSingleTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderSingleTreeIterator.GetNextCursor: TJclSingleTreeNode;
var
  LastRet: TJclSingleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclSingleTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclSingleTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TJclPreOrderSingleTreeIterator.GetNextSibling: TJclSingleTreeNode;
var
  LastRet: TJclSingleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclSingleTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TJclPreOrderSingleTreeIterator.GetPreviousCursor: TJclSingleTreeNode;
var
  LastRet: TJclSingleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclSingleTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclSingleTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TJclPostOrderSingleTreeIterator } ==================================================

function TJclPostOrderSingleTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderSingleTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderSingleTreeIterator.GetNextCursor: TJclSingleTreeNode;
var
  LastRet: TJclSingleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclSingleTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclSingleTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderSingleTreeIterator.GetNextSibling: TJclSingleTreeNode;
var
  LastRet: TJclSingleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclSingleTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclSingleTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderSingleTreeIterator.GetPreviousCursor: TJclSingleTreeNode;
var
  LastRet: TJclSingleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclSingleTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclSingleTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

//=== { TJclDoubleTreeNode } =======================================================

function TJclDoubleTreeNode.IndexOfChild(AChild: TJclDoubleTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclDoubleTreeNode.IndexOfValue(const AValue: Double;
  const AEqualityComparer: IJclDoubleEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclDoubleTreeNode(Children[Result]).Value, AValue) then
      Exit;
  Result := -1;
end;

//=== { TJclDoubleTree } =======================================================

constructor TJclDoubleTree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclDoubleTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclDoubleTree.Add(const AValue: Double): Boolean;
var
  NewNode: TJclDoubleTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AValue, 0.0);

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AValue)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclDoubleTreeNode.Create;
            NewNode.Value := AValue;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclDoubleTreeNode.Create;
        FRoot.Value := AValue;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTree.AddAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclDoubleTree;
  ACollection: IJclDoubleCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclDoubleTree then
  begin
    ADest := TJclDoubleTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclDoubleCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclDoubleTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclDoubleTree then
    TJclDoubleTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclDoubleTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      RemoveNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTree.CloneNode(Node, Parent: TJclDoubleTreeNode): TJclDoubleTreeNode;
var
  Index: Integer;
begin
  Result := TJclDoubleTreeNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  SetLength(Result.Children, Node.ChildrenCount);
  Result.ChildrenCount := Node.ChildrenCount;
  for Index := 0 to Node.ChildrenCount - 1 do
    Result.Children[Index] := CloneNode(TJclDoubleTreeNode(Node.Children[Index]), Result); // recursive call
end;

function TJclDoubleTree.CollectionEquals(const ACollection: IJclDoubleCollection): Boolean;
var
  It, ItSelf: IJclDoubleIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTree.Contains(const AValue: Double): Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AValue)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTree.ContainsAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTree.Extract(const AValue: Double): Boolean;
var
  It: IJclDoubleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AValue) then
      begin
        It.Extract;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTree.ExtractAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleTree.ExtractNode(var ANode: TJclDoubleTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclDoubleTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ExtractNode(TJclDoubleTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ExtractNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclDoubleTree.First: IJclDoubleIterator;
var
  Start: TJclDoubleTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderDoubleTreeIterator.Create(Self, Start, False, isFirst);
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclDoubleTreeNode(Start.Children[0]);
          Result := TJclPostOrderDoubleTreeIterator.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclDoubleTree.GetEnumerator: IJclDoubleIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclDoubleTree.GetRoot: IJclDoubleTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderDoubleTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderDoubleTreeIterator.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclDoubleTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleTree.Last: IJclDoubleIterator;
var
  Start: TJclDoubleTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while Start.ChildrenCount > 0 do
              Start := TJclDoubleTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TJclPreOrderDoubleTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderDoubleTreeIterator.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTree.NodeContains(ANode: TJclDoubleTreeNode; const AValue: Double): Boolean;
var
  Index: Integer;
begin
  Result := ItemsEqual(ANode.Value, AValue);
  if not Result then
    for Index := 0 to ANode.ChildrenCount - 1 do
  begin
    Result := NodeContains(TJclDoubleTreeNode(ANode.Children[Index]), AValue);
    if Result then
      Break;
  end;
end;

procedure TJclDoubleTree.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleTree.PackNode(ANode: TJclDoubleTreeNode);
var
  Index: Integer;
begin
  SetLength(ANode.Children, ANode.ChildrenCount);
  for Index := 0 to ANode.ChildrenCount - 1 do
    PackNode(TJclDoubleTreeNode(ANode.Children[Index]));
end;

function TJclDoubleTree.Remove(const AValue: Double): Boolean;
var
  Extracted: Double;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AValue);
    if Result then
    begin
      Extracted := AValue;
      FreeDouble(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTree.RemoveAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleTree.RemoveNode(var ANode: TJclDoubleTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclDoubleTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    RemoveNode(TJclDoubleTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    RemoveNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeDouble(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclDoubleTree.RetainAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclDoubleTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclDoubleTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclDoubleTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleTree.Create;
  AssignPropertiesTo(Result);
end;

//=== { TJclDoubleTreeIterator } ===========================================================

constructor TJclDoubleTreeIterator.Create(OwnTree: TJclDoubleTree; ACursor: TJclDoubleTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclDoubleEqualityComparer;
end;

function TJclDoubleTreeIterator.Add(const AValue: Double): Boolean;
var
  ParentNode, NewNode: TJclDoubleTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclDoubleTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclDoubleTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTreeIterator.AddChild(const AValue: Double): Boolean;
var
  NewNode: TJclDoubleTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclDoubleTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclDoubleTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclDoubleTreeIterator then
  begin
    ADest := TJclDoubleTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclDoubleTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleTreeIterator.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.RemoveNode(TJclDoubleTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.RemoveNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleTreeIterator.DeleteChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.RemoveNode(TJclDoubleTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.RemoveNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleTreeIterator.Extract;
var
  OldCursor: TJclDoubleTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ExtractNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleTreeIterator.ExtractChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ExtractNode(TJclDoubleTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ExtractNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleTreeIterator.ExtractChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ExtractNode(TJclDoubleTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ExtractNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTreeIterator.GetChild(Index: Integer): Double;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclDoubleTreeNode(FCursor.Children[Index]);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTreeIterator.GetValue: Double;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := 0.0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTreeIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTreeIterator.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTreeIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTreeIterator.IndexOfChild(const AValue: Double): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AValue, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTreeIterator.Insert(const AValue: Double): Boolean;
var
  ParentNode, NewNode: TJclDoubleTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclDoubleTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclDoubleTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTreeIterator.InsertChild(Index: Integer; const AValue: Double): Boolean;
var
  NewNode: TJclDoubleTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclDoubleTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclDoubleTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTreeIterator.IteratorEquals(const AIterator: IJclDoubleIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclDoubleTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclDoubleTreeIterator then
  begin
    ItrObj := TJclDoubleTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclDoubleTreeIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclDoubleTreeIterator.Next: Double;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := 0.0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclDoubleTreeIterator.Parent: Double;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTreeIterator.Previous: Double;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := 0.0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclDoubleTreeIterator.Remove;
var
  OldCursor: TJclDoubleTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.RemoveNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleTreeIterator.Reset;
var
  NewCursor: TJclDoubleTreeNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleTreeIterator.SetChild(Index: Integer; const AValue: Double);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclDoubleTreeNode(FCursor.Children[Index]).Value := AValue
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleTreeIterator.SetValue(const AValue: Double);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeDouble(FCursor.Value);
      FCursor.Value := AValue;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclPreOrderDoubleTreeIterator } ===================================================

function TJclPreOrderDoubleTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderDoubleTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderDoubleTreeIterator.GetNextCursor: TJclDoubleTreeNode;
var
  LastRet: TJclDoubleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclDoubleTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclDoubleTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TJclPreOrderDoubleTreeIterator.GetNextSibling: TJclDoubleTreeNode;
var
  LastRet: TJclDoubleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclDoubleTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TJclPreOrderDoubleTreeIterator.GetPreviousCursor: TJclDoubleTreeNode;
var
  LastRet: TJclDoubleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclDoubleTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclDoubleTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TJclPostOrderDoubleTreeIterator } ==================================================

function TJclPostOrderDoubleTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderDoubleTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderDoubleTreeIterator.GetNextCursor: TJclDoubleTreeNode;
var
  LastRet: TJclDoubleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclDoubleTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclDoubleTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderDoubleTreeIterator.GetNextSibling: TJclDoubleTreeNode;
var
  LastRet: TJclDoubleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclDoubleTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclDoubleTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderDoubleTreeIterator.GetPreviousCursor: TJclDoubleTreeNode;
var
  LastRet: TJclDoubleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclDoubleTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclDoubleTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

//=== { TJclExtendedTreeNode } =======================================================

function TJclExtendedTreeNode.IndexOfChild(AChild: TJclExtendedTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclExtendedTreeNode.IndexOfValue(const AValue: Extended;
  const AEqualityComparer: IJclExtendedEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclExtendedTreeNode(Children[Result]).Value, AValue) then
      Exit;
  Result := -1;
end;

//=== { TJclExtendedTree } =======================================================

constructor TJclExtendedTree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclExtendedTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclExtendedTree.Add(const AValue: Extended): Boolean;
var
  NewNode: TJclExtendedTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AValue, 0.0);

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AValue)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclExtendedTreeNode.Create;
            NewNode.Value := AValue;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclExtendedTreeNode.Create;
        FRoot.Value := AValue;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTree.AddAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclExtendedTree;
  ACollection: IJclExtendedCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclExtendedTree then
  begin
    ADest := TJclExtendedTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclExtendedCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclExtendedTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclExtendedTree then
    TJclExtendedTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclExtendedTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      RemoveNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTree.CloneNode(Node, Parent: TJclExtendedTreeNode): TJclExtendedTreeNode;
var
  Index: Integer;
begin
  Result := TJclExtendedTreeNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  SetLength(Result.Children, Node.ChildrenCount);
  Result.ChildrenCount := Node.ChildrenCount;
  for Index := 0 to Node.ChildrenCount - 1 do
    Result.Children[Index] := CloneNode(TJclExtendedTreeNode(Node.Children[Index]), Result); // recursive call
end;

function TJclExtendedTree.CollectionEquals(const ACollection: IJclExtendedCollection): Boolean;
var
  It, ItSelf: IJclExtendedIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTree.Contains(const AValue: Extended): Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AValue)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTree.ContainsAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTree.Extract(const AValue: Extended): Boolean;
var
  It: IJclExtendedIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AValue) then
      begin
        It.Extract;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTree.ExtractAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedTree.ExtractNode(var ANode: TJclExtendedTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclExtendedTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ExtractNode(TJclExtendedTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ExtractNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclExtendedTree.First: IJclExtendedIterator;
var
  Start: TJclExtendedTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderExtendedTreeIterator.Create(Self, Start, False, isFirst);
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclExtendedTreeNode(Start.Children[0]);
          Result := TJclPostOrderExtendedTreeIterator.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclExtendedTree.GetEnumerator: IJclExtendedIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclExtendedTree.GetRoot: IJclExtendedTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderExtendedTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderExtendedTreeIterator.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclExtendedTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedTree.Last: IJclExtendedIterator;
var
  Start: TJclExtendedTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while Start.ChildrenCount > 0 do
              Start := TJclExtendedTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TJclPreOrderExtendedTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderExtendedTreeIterator.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTree.NodeContains(ANode: TJclExtendedTreeNode; const AValue: Extended): Boolean;
var
  Index: Integer;
begin
  Result := ItemsEqual(ANode.Value, AValue);
  if not Result then
    for Index := 0 to ANode.ChildrenCount - 1 do
  begin
    Result := NodeContains(TJclExtendedTreeNode(ANode.Children[Index]), AValue);
    if Result then
      Break;
  end;
end;

procedure TJclExtendedTree.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedTree.PackNode(ANode: TJclExtendedTreeNode);
var
  Index: Integer;
begin
  SetLength(ANode.Children, ANode.ChildrenCount);
  for Index := 0 to ANode.ChildrenCount - 1 do
    PackNode(TJclExtendedTreeNode(ANode.Children[Index]));
end;

function TJclExtendedTree.Remove(const AValue: Extended): Boolean;
var
  Extracted: Extended;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AValue);
    if Result then
    begin
      Extracted := AValue;
      FreeExtended(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTree.RemoveAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedTree.RemoveNode(var ANode: TJclExtendedTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclExtendedTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    RemoveNode(TJclExtendedTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    RemoveNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeExtended(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclExtendedTree.RetainAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclExtendedTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclExtendedTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclExtendedTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedTree.Create;
  AssignPropertiesTo(Result);
end;

//=== { TJclExtendedTreeIterator } ===========================================================

constructor TJclExtendedTreeIterator.Create(OwnTree: TJclExtendedTree; ACursor: TJclExtendedTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclExtendedEqualityComparer;
end;

function TJclExtendedTreeIterator.Add(const AValue: Extended): Boolean;
var
  ParentNode, NewNode: TJclExtendedTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclExtendedTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclExtendedTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTreeIterator.AddChild(const AValue: Extended): Boolean;
var
  NewNode: TJclExtendedTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclExtendedTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclExtendedTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclExtendedTreeIterator then
  begin
    ADest := TJclExtendedTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclExtendedTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedTreeIterator.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.RemoveNode(TJclExtendedTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.RemoveNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedTreeIterator.DeleteChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.RemoveNode(TJclExtendedTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.RemoveNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedTreeIterator.Extract;
var
  OldCursor: TJclExtendedTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ExtractNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedTreeIterator.ExtractChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ExtractNode(TJclExtendedTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ExtractNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedTreeIterator.ExtractChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ExtractNode(TJclExtendedTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ExtractNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTreeIterator.GetChild(Index: Integer): Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclExtendedTreeNode(FCursor.Children[Index]);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTreeIterator.GetValue: Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := 0.0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTreeIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTreeIterator.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTreeIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTreeIterator.IndexOfChild(const AValue: Extended): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AValue, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTreeIterator.Insert(const AValue: Extended): Boolean;
var
  ParentNode, NewNode: TJclExtendedTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclExtendedTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclExtendedTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTreeIterator.InsertChild(Index: Integer; const AValue: Extended): Boolean;
var
  NewNode: TJclExtendedTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclExtendedTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclExtendedTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTreeIterator.IteratorEquals(const AIterator: IJclExtendedIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclExtendedTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclExtendedTreeIterator then
  begin
    ItrObj := TJclExtendedTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclExtendedTreeIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclExtendedTreeIterator.Next: Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := 0.0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclExtendedTreeIterator.Parent: Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTreeIterator.Previous: Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := 0.0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclExtendedTreeIterator.Remove;
var
  OldCursor: TJclExtendedTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.RemoveNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedTreeIterator.Reset;
var
  NewCursor: TJclExtendedTreeNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedTreeIterator.SetChild(Index: Integer; const AValue: Extended);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclExtendedTreeNode(FCursor.Children[Index]).Value := AValue
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedTreeIterator.SetValue(const AValue: Extended);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeExtended(FCursor.Value);
      FCursor.Value := AValue;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclPreOrderExtendedTreeIterator } ===================================================

function TJclPreOrderExtendedTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderExtendedTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderExtendedTreeIterator.GetNextCursor: TJclExtendedTreeNode;
var
  LastRet: TJclExtendedTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclExtendedTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclExtendedTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TJclPreOrderExtendedTreeIterator.GetNextSibling: TJclExtendedTreeNode;
var
  LastRet: TJclExtendedTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclExtendedTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TJclPreOrderExtendedTreeIterator.GetPreviousCursor: TJclExtendedTreeNode;
var
  LastRet: TJclExtendedTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclExtendedTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclExtendedTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TJclPostOrderExtendedTreeIterator } ==================================================

function TJclPostOrderExtendedTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderExtendedTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderExtendedTreeIterator.GetNextCursor: TJclExtendedTreeNode;
var
  LastRet: TJclExtendedTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclExtendedTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclExtendedTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderExtendedTreeIterator.GetNextSibling: TJclExtendedTreeNode;
var
  LastRet: TJclExtendedTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclExtendedTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclExtendedTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderExtendedTreeIterator.GetPreviousCursor: TJclExtendedTreeNode;
var
  LastRet: TJclExtendedTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclExtendedTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclExtendedTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

//=== { TJclIntegerTreeNode } =======================================================

function TJclIntegerTreeNode.IndexOfChild(AChild: TJclIntegerTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclIntegerTreeNode.IndexOfValue(AValue: Integer;
  const AEqualityComparer: IJclIntegerEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclIntegerTreeNode(Children[Result]).Value, AValue) then
      Exit;
  Result := -1;
end;

//=== { TJclIntegerTree } =======================================================

constructor TJclIntegerTree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclIntegerTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclIntegerTree.Add(AValue: Integer): Boolean;
var
  NewNode: TJclIntegerTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AValue, 0);

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AValue)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclIntegerTreeNode.Create;
            NewNode.Value := AValue;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclIntegerTreeNode.Create;
        FRoot.Value := AValue;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTree.AddAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclIntegerTree;
  ACollection: IJclIntegerCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntegerTree then
  begin
    ADest := TJclIntegerTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclIntegerCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclIntegerTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntegerTree then
    TJclIntegerTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclIntegerTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      RemoveNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTree.CloneNode(Node, Parent: TJclIntegerTreeNode): TJclIntegerTreeNode;
var
  Index: Integer;
begin
  Result := TJclIntegerTreeNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  SetLength(Result.Children, Node.ChildrenCount);
  Result.ChildrenCount := Node.ChildrenCount;
  for Index := 0 to Node.ChildrenCount - 1 do
    Result.Children[Index] := CloneNode(TJclIntegerTreeNode(Node.Children[Index]), Result); // recursive call
end;

function TJclIntegerTree.CollectionEquals(const ACollection: IJclIntegerCollection): Boolean;
var
  It, ItSelf: IJclIntegerIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTree.Contains(AValue: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AValue)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTree.ContainsAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTree.Extract(AValue: Integer): Boolean;
var
  It: IJclIntegerIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AValue) then
      begin
        It.Extract;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTree.ExtractAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerTree.ExtractNode(var ANode: TJclIntegerTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclIntegerTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ExtractNode(TJclIntegerTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ExtractNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclIntegerTree.First: IJclIntegerIterator;
var
  Start: TJclIntegerTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderIntegerTreeIterator.Create(Self, Start, False, isFirst);
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclIntegerTreeNode(Start.Children[0]);
          Result := TJclPostOrderIntegerTreeIterator.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntegerTree.GetEnumerator: IJclIntegerIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntegerTree.GetRoot: IJclIntegerTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderIntegerTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderIntegerTreeIterator.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclIntegerTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerTree.Last: IJclIntegerIterator;
var
  Start: TJclIntegerTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while Start.ChildrenCount > 0 do
              Start := TJclIntegerTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TJclPreOrderIntegerTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderIntegerTreeIterator.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTree.NodeContains(ANode: TJclIntegerTreeNode; AValue: Integer): Boolean;
var
  Index: Integer;
begin
  Result := ItemsEqual(ANode.Value, AValue);
  if not Result then
    for Index := 0 to ANode.ChildrenCount - 1 do
  begin
    Result := NodeContains(TJclIntegerTreeNode(ANode.Children[Index]), AValue);
    if Result then
      Break;
  end;
end;

procedure TJclIntegerTree.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerTree.PackNode(ANode: TJclIntegerTreeNode);
var
  Index: Integer;
begin
  SetLength(ANode.Children, ANode.ChildrenCount);
  for Index := 0 to ANode.ChildrenCount - 1 do
    PackNode(TJclIntegerTreeNode(ANode.Children[Index]));
end;

function TJclIntegerTree.Remove(AValue: Integer): Boolean;
var
  Extracted: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AValue);
    if Result then
    begin
      Extracted := AValue;
      FreeInteger(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTree.RemoveAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerTree.RemoveNode(var ANode: TJclIntegerTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclIntegerTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    RemoveNode(TJclIntegerTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    RemoveNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeInteger(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclIntegerTree.RetainAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntegerTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclIntegerTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntegerTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerTree.Create;
  AssignPropertiesTo(Result);
end;

//=== { TJclIntegerTreeIterator } ===========================================================

constructor TJclIntegerTreeIterator.Create(OwnTree: TJclIntegerTree; ACursor: TJclIntegerTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclIntegerEqualityComparer;
end;

function TJclIntegerTreeIterator.Add(AValue: Integer): Boolean;
var
  ParentNode, NewNode: TJclIntegerTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclIntegerTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclIntegerTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTreeIterator.AddChild(AValue: Integer): Boolean;
var
  NewNode: TJclIntegerTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclIntegerTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclIntegerTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclIntegerTreeIterator then
  begin
    ADest := TJclIntegerTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclIntegerTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerTreeIterator.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.RemoveNode(TJclIntegerTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.RemoveNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerTreeIterator.DeleteChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.RemoveNode(TJclIntegerTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.RemoveNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerTreeIterator.Extract;
var
  OldCursor: TJclIntegerTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ExtractNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerTreeIterator.ExtractChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ExtractNode(TJclIntegerTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ExtractNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerTreeIterator.ExtractChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ExtractNode(TJclIntegerTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ExtractNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTreeIterator.GetChild(Index: Integer): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclIntegerTreeNode(FCursor.Children[Index]);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTreeIterator.GetValue: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := 0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTreeIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTreeIterator.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTreeIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTreeIterator.IndexOfChild(AValue: Integer): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AValue, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTreeIterator.Insert(AValue: Integer): Boolean;
var
  ParentNode, NewNode: TJclIntegerTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclIntegerTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclIntegerTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTreeIterator.InsertChild(Index: Integer; AValue: Integer): Boolean;
var
  NewNode: TJclIntegerTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclIntegerTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclIntegerTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTreeIterator.IteratorEquals(const AIterator: IJclIntegerIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclIntegerTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclIntegerTreeIterator then
  begin
    ItrObj := TJclIntegerTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntegerTreeIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntegerTreeIterator.Next: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := 0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclIntegerTreeIterator.Parent: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTreeIterator.Previous: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := 0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntegerTreeIterator.Remove;
var
  OldCursor: TJclIntegerTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.RemoveNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerTreeIterator.Reset;
var
  NewCursor: TJclIntegerTreeNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerTreeIterator.SetChild(Index: Integer; AValue: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclIntegerTreeNode(FCursor.Children[Index]).Value := AValue
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerTreeIterator.SetValue(AValue: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeInteger(FCursor.Value);
      FCursor.Value := AValue;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclPreOrderIntegerTreeIterator } ===================================================

function TJclPreOrderIntegerTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderIntegerTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderIntegerTreeIterator.GetNextCursor: TJclIntegerTreeNode;
var
  LastRet: TJclIntegerTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclIntegerTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclIntegerTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TJclPreOrderIntegerTreeIterator.GetNextSibling: TJclIntegerTreeNode;
var
  LastRet: TJclIntegerTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclIntegerTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TJclPreOrderIntegerTreeIterator.GetPreviousCursor: TJclIntegerTreeNode;
var
  LastRet: TJclIntegerTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclIntegerTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclIntegerTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TJclPostOrderIntegerTreeIterator } ==================================================

function TJclPostOrderIntegerTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderIntegerTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderIntegerTreeIterator.GetNextCursor: TJclIntegerTreeNode;
var
  LastRet: TJclIntegerTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclIntegerTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclIntegerTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderIntegerTreeIterator.GetNextSibling: TJclIntegerTreeNode;
var
  LastRet: TJclIntegerTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclIntegerTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclIntegerTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderIntegerTreeIterator.GetPreviousCursor: TJclIntegerTreeNode;
var
  LastRet: TJclIntegerTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclIntegerTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclIntegerTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

//=== { TJclCardinalTreeNode } =======================================================

function TJclCardinalTreeNode.IndexOfChild(AChild: TJclCardinalTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclCardinalTreeNode.IndexOfValue(AValue: Cardinal;
  const AEqualityComparer: IJclCardinalEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclCardinalTreeNode(Children[Result]).Value, AValue) then
      Exit;
  Result := -1;
end;

//=== { TJclCardinalTree } =======================================================

constructor TJclCardinalTree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclCardinalTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclCardinalTree.Add(AValue: Cardinal): Boolean;
var
  NewNode: TJclCardinalTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AValue, 0);

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AValue)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclCardinalTreeNode.Create;
            NewNode.Value := AValue;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclCardinalTreeNode.Create;
        FRoot.Value := AValue;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTree.AddAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclCardinalTree;
  ACollection: IJclCardinalCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclCardinalTree then
  begin
    ADest := TJclCardinalTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclCardinalCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclCardinalTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclCardinalTree then
    TJclCardinalTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclCardinalTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      RemoveNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTree.CloneNode(Node, Parent: TJclCardinalTreeNode): TJclCardinalTreeNode;
var
  Index: Integer;
begin
  Result := TJclCardinalTreeNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  SetLength(Result.Children, Node.ChildrenCount);
  Result.ChildrenCount := Node.ChildrenCount;
  for Index := 0 to Node.ChildrenCount - 1 do
    Result.Children[Index] := CloneNode(TJclCardinalTreeNode(Node.Children[Index]), Result); // recursive call
end;

function TJclCardinalTree.CollectionEquals(const ACollection: IJclCardinalCollection): Boolean;
var
  It, ItSelf: IJclCardinalIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTree.Contains(AValue: Cardinal): Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AValue)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTree.ContainsAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTree.Extract(AValue: Cardinal): Boolean;
var
  It: IJclCardinalIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AValue) then
      begin
        It.Extract;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTree.ExtractAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalTree.ExtractNode(var ANode: TJclCardinalTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclCardinalTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ExtractNode(TJclCardinalTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ExtractNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclCardinalTree.First: IJclCardinalIterator;
var
  Start: TJclCardinalTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderCardinalTreeIterator.Create(Self, Start, False, isFirst);
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclCardinalTreeNode(Start.Children[0]);
          Result := TJclPostOrderCardinalTreeIterator.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclCardinalTree.GetEnumerator: IJclCardinalIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclCardinalTree.GetRoot: IJclCardinalTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderCardinalTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderCardinalTreeIterator.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclCardinalTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalTree.Last: IJclCardinalIterator;
var
  Start: TJclCardinalTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while Start.ChildrenCount > 0 do
              Start := TJclCardinalTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TJclPreOrderCardinalTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderCardinalTreeIterator.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTree.NodeContains(ANode: TJclCardinalTreeNode; AValue: Cardinal): Boolean;
var
  Index: Integer;
begin
  Result := ItemsEqual(ANode.Value, AValue);
  if not Result then
    for Index := 0 to ANode.ChildrenCount - 1 do
  begin
    Result := NodeContains(TJclCardinalTreeNode(ANode.Children[Index]), AValue);
    if Result then
      Break;
  end;
end;

procedure TJclCardinalTree.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalTree.PackNode(ANode: TJclCardinalTreeNode);
var
  Index: Integer;
begin
  SetLength(ANode.Children, ANode.ChildrenCount);
  for Index := 0 to ANode.ChildrenCount - 1 do
    PackNode(TJclCardinalTreeNode(ANode.Children[Index]));
end;

function TJclCardinalTree.Remove(AValue: Cardinal): Boolean;
var
  Extracted: Cardinal;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AValue);
    if Result then
    begin
      Extracted := AValue;
      FreeCardinal(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTree.RemoveAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalTree.RemoveNode(var ANode: TJclCardinalTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclCardinalTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    RemoveNode(TJclCardinalTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    RemoveNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeCardinal(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclCardinalTree.RetainAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclCardinalTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclCardinalTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclCardinalTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalTree.Create;
  AssignPropertiesTo(Result);
end;

//=== { TJclCardinalTreeIterator } ===========================================================

constructor TJclCardinalTreeIterator.Create(OwnTree: TJclCardinalTree; ACursor: TJclCardinalTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclCardinalEqualityComparer;
end;

function TJclCardinalTreeIterator.Add(AValue: Cardinal): Boolean;
var
  ParentNode, NewNode: TJclCardinalTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclCardinalTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclCardinalTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTreeIterator.AddChild(AValue: Cardinal): Boolean;
var
  NewNode: TJclCardinalTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclCardinalTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclCardinalTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclCardinalTreeIterator then
  begin
    ADest := TJclCardinalTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclCardinalTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalTreeIterator.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.RemoveNode(TJclCardinalTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.RemoveNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalTreeIterator.DeleteChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.RemoveNode(TJclCardinalTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.RemoveNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalTreeIterator.Extract;
var
  OldCursor: TJclCardinalTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ExtractNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalTreeIterator.ExtractChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ExtractNode(TJclCardinalTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ExtractNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalTreeIterator.ExtractChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ExtractNode(TJclCardinalTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ExtractNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTreeIterator.GetChild(Index: Integer): Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclCardinalTreeNode(FCursor.Children[Index]);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTreeIterator.GetValue: Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := 0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTreeIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTreeIterator.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTreeIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTreeIterator.IndexOfChild(AValue: Cardinal): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AValue, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTreeIterator.Insert(AValue: Cardinal): Boolean;
var
  ParentNode, NewNode: TJclCardinalTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclCardinalTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclCardinalTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTreeIterator.InsertChild(Index: Integer; AValue: Cardinal): Boolean;
var
  NewNode: TJclCardinalTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclCardinalTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclCardinalTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTreeIterator.IteratorEquals(const AIterator: IJclCardinalIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclCardinalTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclCardinalTreeIterator then
  begin
    ItrObj := TJclCardinalTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclCardinalTreeIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclCardinalTreeIterator.Next: Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := 0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclCardinalTreeIterator.Parent: Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTreeIterator.Previous: Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := 0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclCardinalTreeIterator.Remove;
var
  OldCursor: TJclCardinalTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.RemoveNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalTreeIterator.Reset;
var
  NewCursor: TJclCardinalTreeNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalTreeIterator.SetChild(Index: Integer; AValue: Cardinal);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclCardinalTreeNode(FCursor.Children[Index]).Value := AValue
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalTreeIterator.SetValue(AValue: Cardinal);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeCardinal(FCursor.Value);
      FCursor.Value := AValue;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclPreOrderCardinalTreeIterator } ===================================================

function TJclPreOrderCardinalTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderCardinalTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderCardinalTreeIterator.GetNextCursor: TJclCardinalTreeNode;
var
  LastRet: TJclCardinalTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclCardinalTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclCardinalTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TJclPreOrderCardinalTreeIterator.GetNextSibling: TJclCardinalTreeNode;
var
  LastRet: TJclCardinalTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclCardinalTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TJclPreOrderCardinalTreeIterator.GetPreviousCursor: TJclCardinalTreeNode;
var
  LastRet: TJclCardinalTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclCardinalTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclCardinalTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TJclPostOrderCardinalTreeIterator } ==================================================

function TJclPostOrderCardinalTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderCardinalTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderCardinalTreeIterator.GetNextCursor: TJclCardinalTreeNode;
var
  LastRet: TJclCardinalTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclCardinalTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclCardinalTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderCardinalTreeIterator.GetNextSibling: TJclCardinalTreeNode;
var
  LastRet: TJclCardinalTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclCardinalTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclCardinalTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderCardinalTreeIterator.GetPreviousCursor: TJclCardinalTreeNode;
var
  LastRet: TJclCardinalTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclCardinalTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclCardinalTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

//=== { TJclInt64TreeNode } =======================================================

function TJclInt64TreeNode.IndexOfChild(AChild: TJclInt64TreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclInt64TreeNode.IndexOfValue(const AValue: Int64;
  const AEqualityComparer: IJclInt64EqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclInt64TreeNode(Children[Result]).Value, AValue) then
      Exit;
  Result := -1;
end;

//=== { TJclInt64Tree } =======================================================

constructor TJclInt64Tree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclInt64Tree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclInt64Tree.Add(const AValue: Int64): Boolean;
var
  NewNode: TJclInt64TreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AValue, 0);

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AValue)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclInt64TreeNode.Create;
            NewNode.Value := AValue;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclInt64TreeNode.Create;
        FRoot.Value := AValue;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Tree.AddAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Tree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclInt64Tree;
  ACollection: IJclInt64Collection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclInt64Tree then
  begin
    ADest := TJclInt64Tree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclInt64Collection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclInt64Tree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclInt64Tree then
    TJclInt64Tree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclInt64Tree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      RemoveNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Tree.CloneNode(Node, Parent: TJclInt64TreeNode): TJclInt64TreeNode;
var
  Index: Integer;
begin
  Result := TJclInt64TreeNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  SetLength(Result.Children, Node.ChildrenCount);
  Result.ChildrenCount := Node.ChildrenCount;
  for Index := 0 to Node.ChildrenCount - 1 do
    Result.Children[Index] := CloneNode(TJclInt64TreeNode(Node.Children[Index]), Result); // recursive call
end;

function TJclInt64Tree.CollectionEquals(const ACollection: IJclInt64Collection): Boolean;
var
  It, ItSelf: IJclInt64Iterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Tree.Contains(const AValue: Int64): Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AValue)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Tree.ContainsAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Tree.Extract(const AValue: Int64): Boolean;
var
  It: IJclInt64Iterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AValue) then
      begin
        It.Extract;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Tree.ExtractAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Tree.ExtractNode(var ANode: TJclInt64TreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclInt64TreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ExtractNode(TJclInt64TreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ExtractNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclInt64Tree.First: IJclInt64Iterator;
var
  Start: TJclInt64TreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderInt64TreeIterator.Create(Self, Start, False, isFirst);
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclInt64TreeNode(Start.Children[0]);
          Result := TJclPostOrderInt64TreeIterator.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclInt64Tree.GetEnumerator: IJclInt64Iterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclInt64Tree.GetRoot: IJclInt64TreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderInt64TreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderInt64TreeIterator.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Tree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclInt64Tree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64Tree.Last: IJclInt64Iterator;
var
  Start: TJclInt64TreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while Start.ChildrenCount > 0 do
              Start := TJclInt64TreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TJclPreOrderInt64TreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderInt64TreeIterator.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Tree.NodeContains(ANode: TJclInt64TreeNode; const AValue: Int64): Boolean;
var
  Index: Integer;
begin
  Result := ItemsEqual(ANode.Value, AValue);
  if not Result then
    for Index := 0 to ANode.ChildrenCount - 1 do
  begin
    Result := NodeContains(TJclInt64TreeNode(ANode.Children[Index]), AValue);
    if Result then
      Break;
  end;
end;

procedure TJclInt64Tree.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Tree.PackNode(ANode: TJclInt64TreeNode);
var
  Index: Integer;
begin
  SetLength(ANode.Children, ANode.ChildrenCount);
  for Index := 0 to ANode.ChildrenCount - 1 do
    PackNode(TJclInt64TreeNode(ANode.Children[Index]));
end;

function TJclInt64Tree.Remove(const AValue: Int64): Boolean;
var
  Extracted: Int64;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AValue);
    if Result then
    begin
      Extracted := AValue;
      FreeInt64(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Tree.RemoveAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Tree.RemoveNode(var ANode: TJclInt64TreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclInt64TreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    RemoveNode(TJclInt64TreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    RemoveNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeInt64(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclInt64Tree.RetainAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Tree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclInt64Tree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclInt64Tree.Size: Integer;
begin
  Result := FSize;
end;

function TJclInt64Tree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64Tree.Create;
  AssignPropertiesTo(Result);
end;

//=== { TJclInt64TreeIterator } ===========================================================

constructor TJclInt64TreeIterator.Create(OwnTree: TJclInt64Tree; ACursor: TJclInt64TreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclInt64EqualityComparer;
end;

function TJclInt64TreeIterator.Add(const AValue: Int64): Boolean;
var
  ParentNode, NewNode: TJclInt64TreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclInt64Tree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclInt64TreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64TreeIterator.AddChild(const AValue: Int64): Boolean;
var
  NewNode: TJclInt64TreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclInt64TreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64TreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclInt64TreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclInt64TreeIterator then
  begin
    ADest := TJclInt64TreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclInt64TreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64TreeIterator.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.RemoveNode(TJclInt64TreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.RemoveNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64TreeIterator.DeleteChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.RemoveNode(TJclInt64TreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.RemoveNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64TreeIterator.Extract;
var
  OldCursor: TJclInt64TreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ExtractNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64TreeIterator.ExtractChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ExtractNode(TJclInt64TreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ExtractNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64TreeIterator.ExtractChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ExtractNode(TJclInt64TreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ExtractNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64TreeIterator.GetChild(Index: Integer): Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclInt64TreeNode(FCursor.Children[Index]);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64TreeIterator.GetValue: Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := 0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64TreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64TreeIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64TreeIterator.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64TreeIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64TreeIterator.IndexOfChild(const AValue: Int64): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AValue, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64TreeIterator.Insert(const AValue: Int64): Boolean;
var
  ParentNode, NewNode: TJclInt64TreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclInt64Tree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclInt64TreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64TreeIterator.InsertChild(Index: Integer; const AValue: Int64): Boolean;
var
  NewNode: TJclInt64TreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclInt64Tree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclInt64TreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64TreeIterator.IteratorEquals(const AIterator: IJclInt64Iterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclInt64TreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclInt64TreeIterator then
  begin
    ItrObj := TJclInt64TreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclInt64TreeIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclInt64TreeIterator.Next: Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := 0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64TreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclInt64TreeIterator.Parent: Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64TreeIterator.Previous: Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := 0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64TreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclInt64TreeIterator.Remove;
var
  OldCursor: TJclInt64TreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.RemoveNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64TreeIterator.Reset;
var
  NewCursor: TJclInt64TreeNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64TreeIterator.SetChild(Index: Integer; const AValue: Int64);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclInt64TreeNode(FCursor.Children[Index]).Value := AValue
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64TreeIterator.SetValue(const AValue: Int64);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeInt64(FCursor.Value);
      FCursor.Value := AValue;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclPreOrderInt64TreeIterator } ===================================================

function TJclPreOrderInt64TreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderInt64TreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderInt64TreeIterator.GetNextCursor: TJclInt64TreeNode;
var
  LastRet: TJclInt64TreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclInt64TreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclInt64TreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TJclPreOrderInt64TreeIterator.GetNextSibling: TJclInt64TreeNode;
var
  LastRet: TJclInt64TreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclInt64TreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TJclPreOrderInt64TreeIterator.GetPreviousCursor: TJclInt64TreeNode;
var
  LastRet: TJclInt64TreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclInt64TreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclInt64TreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TJclPostOrderInt64TreeIterator } ==================================================

function TJclPostOrderInt64TreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderInt64TreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderInt64TreeIterator.GetNextCursor: TJclInt64TreeNode;
var
  LastRet: TJclInt64TreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclInt64TreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclInt64TreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderInt64TreeIterator.GetNextSibling: TJclInt64TreeNode;
var
  LastRet: TJclInt64TreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclInt64TreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclInt64TreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderInt64TreeIterator.GetPreviousCursor: TJclInt64TreeNode;
var
  LastRet: TJclInt64TreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclInt64TreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclInt64TreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

//=== { TJclPtrTreeNode } =======================================================

function TJclPtrTreeNode.IndexOfChild(AChild: TJclPtrTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclPtrTreeNode.IndexOfValue(APtr: Pointer;
  const AEqualityComparer: IJclPtrEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclPtrTreeNode(Children[Result]).Value, APtr) then
      Exit;
  Result := -1;
end;

//=== { TJclPtrTree } =======================================================

constructor TJclPtrTree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclPtrTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclPtrTree.Add(APtr: Pointer): Boolean;
var
  NewNode: TJclPtrTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(APtr, nil);

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(APtr)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclPtrTreeNode.Create;
            NewNode.Value := APtr;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclPtrTreeNode.Create;
        FRoot.Value := APtr;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTree.AddAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclPtrTree;
  ACollection: IJclPtrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclPtrTree then
  begin
    ADest := TJclPtrTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclPtrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclPtrTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclPtrTree then
    TJclPtrTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclPtrTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      RemoveNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTree.CloneNode(Node, Parent: TJclPtrTreeNode): TJclPtrTreeNode;
var
  Index: Integer;
begin
  Result := TJclPtrTreeNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  SetLength(Result.Children, Node.ChildrenCount);
  Result.ChildrenCount := Node.ChildrenCount;
  for Index := 0 to Node.ChildrenCount - 1 do
    Result.Children[Index] := CloneNode(TJclPtrTreeNode(Node.Children[Index]), Result); // recursive call
end;

function TJclPtrTree.CollectionEquals(const ACollection: IJclPtrCollection): Boolean;
var
  It, ItSelf: IJclPtrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTree.Contains(APtr: Pointer): Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, APtr)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTree.ContainsAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTree.Extract(APtr: Pointer): Boolean;
var
  It: IJclPtrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, APtr) then
      begin
        It.Extract;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTree.ExtractAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrTree.ExtractNode(var ANode: TJclPtrTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclPtrTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ExtractNode(TJclPtrTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ExtractNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclPtrTree.First: IJclPtrIterator;
var
  Start: TJclPtrTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderPtrTreeIterator.Create(Self, Start, False, isFirst);
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclPtrTreeNode(Start.Children[0]);
          Result := TJclPostOrderPtrTreeIterator.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclPtrTree.GetEnumerator: IJclPtrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclPtrTree.GetRoot: IJclPtrTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderPtrTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderPtrTreeIterator.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclPtrTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrTree.Last: IJclPtrIterator;
var
  Start: TJclPtrTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while Start.ChildrenCount > 0 do
              Start := TJclPtrTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TJclPreOrderPtrTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderPtrTreeIterator.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTree.NodeContains(ANode: TJclPtrTreeNode; APtr: Pointer): Boolean;
var
  Index: Integer;
begin
  Result := ItemsEqual(ANode.Value, APtr);
  if not Result then
    for Index := 0 to ANode.ChildrenCount - 1 do
  begin
    Result := NodeContains(TJclPtrTreeNode(ANode.Children[Index]), APtr);
    if Result then
      Break;
  end;
end;

procedure TJclPtrTree.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrTree.PackNode(ANode: TJclPtrTreeNode);
var
  Index: Integer;
begin
  SetLength(ANode.Children, ANode.ChildrenCount);
  for Index := 0 to ANode.ChildrenCount - 1 do
    PackNode(TJclPtrTreeNode(ANode.Children[Index]));
end;

function TJclPtrTree.Remove(APtr: Pointer): Boolean;
var
  Extracted: Pointer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(APtr);
    if Result then
    begin
      Extracted := APtr;
      FreePointer(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTree.RemoveAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrTree.RemoveNode(var ANode: TJclPtrTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclPtrTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    RemoveNode(TJclPtrTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    RemoveNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreePointer(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclPtrTree.RetainAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclPtrTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclPtrTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclPtrTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrTree.Create;
  AssignPropertiesTo(Result);
end;

//=== { TJclPtrTreeIterator } ===========================================================

constructor TJclPtrTreeIterator.Create(OwnTree: TJclPtrTree; ACursor: TJclPtrTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclPtrEqualityComparer;
end;

function TJclPtrTreeIterator.Add(APtr: Pointer): Boolean;
var
  ParentNode, NewNode: TJclPtrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclPtrTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(APtr, nil))
      and ((not FOwnTree.Contains(APtr)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclPtrTreeNode.Create;
        NewNode.Value := APtr;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTreeIterator.AddChild(APtr: Pointer): Boolean;
var
  NewNode: TJclPtrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(APtr, nil))
      and ((not FOwnTree.Contains(APtr)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclPtrTreeNode.Create;
        NewNode.Value := APtr;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclPtrTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclPtrTreeIterator then
  begin
    ADest := TJclPtrTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclPtrTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrTreeIterator.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.RemoveNode(TJclPtrTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.RemoveNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrTreeIterator.DeleteChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.RemoveNode(TJclPtrTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.RemoveNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrTreeIterator.Extract;
var
  OldCursor: TJclPtrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ExtractNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrTreeIterator.ExtractChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ExtractNode(TJclPtrTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ExtractNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrTreeIterator.ExtractChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ExtractNode(TJclPtrTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ExtractNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTreeIterator.GetChild(Index: Integer): Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclPtrTreeNode(FCursor.Children[Index]);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTreeIterator.GetPointer: Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTreeIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTreeIterator.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTreeIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTreeIterator.IndexOfChild(APtr: Pointer): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(APtr, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTreeIterator.Insert(APtr: Pointer): Boolean;
var
  ParentNode, NewNode: TJclPtrTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclPtrTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(APtr, nil))
      and ((not FOwnTree.Contains(APtr)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclPtrTreeNode.Create;
        NewNode.Value := APtr;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTreeIterator.InsertChild(Index: Integer; APtr: Pointer): Boolean;
var
  NewNode: TJclPtrTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclPtrTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(APtr, nil))
      and ((not FOwnTree.Contains(APtr)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclPtrTreeNode.Create;
        NewNode.Value := APtr;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTreeIterator.IteratorEquals(const AIterator: IJclPtrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclPtrTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclPtrTreeIterator then
  begin
    ItrObj := TJclPtrTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclPtrTreeIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclPtrTreeIterator.Next: Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclPtrTreeIterator.Parent: Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTreeIterator.Previous: Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclPtrTreeIterator.Remove;
var
  OldCursor: TJclPtrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.RemoveNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrTreeIterator.Reset;
var
  NewCursor: TJclPtrTreeNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrTreeIterator.SetChild(Index: Integer; APtr: Pointer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclPtrTreeNode(FCursor.Children[Index]).Value := APtr
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrTreeIterator.SetPointer(APtr: Pointer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreePointer(FCursor.Value);
      FCursor.Value := APtr;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclPreOrderPtrTreeIterator } ===================================================

function TJclPreOrderPtrTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderPtrTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderPtrTreeIterator.GetNextCursor: TJclPtrTreeNode;
var
  LastRet: TJclPtrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclPtrTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclPtrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TJclPreOrderPtrTreeIterator.GetNextSibling: TJclPtrTreeNode;
var
  LastRet: TJclPtrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclPtrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TJclPreOrderPtrTreeIterator.GetPreviousCursor: TJclPtrTreeNode;
var
  LastRet: TJclPtrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclPtrTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclPtrTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TJclPostOrderPtrTreeIterator } ==================================================

function TJclPostOrderPtrTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderPtrTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderPtrTreeIterator.GetNextCursor: TJclPtrTreeNode;
var
  LastRet: TJclPtrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclPtrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclPtrTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderPtrTreeIterator.GetNextSibling: TJclPtrTreeNode;
var
  LastRet: TJclPtrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclPtrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclPtrTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderPtrTreeIterator.GetPreviousCursor: TJclPtrTreeNode;
var
  LastRet: TJclPtrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclPtrTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclPtrTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

//=== { TJclTreeNode } =======================================================

function TJclTreeNode.IndexOfChild(AChild: TJclTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclTreeNode.IndexOfValue(AObject: TObject;
  const AEqualityComparer: IJclEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclTreeNode(Children[Result]).Value, AObject) then
      Exit;
  Result := -1;
end;

//=== { TJclTree } =======================================================

constructor TJclTree.Create(AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  FTraverseOrder := toPreOrder;
end;

destructor TJclTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclTree.Add(AObject: TObject): Boolean;
var
  NewNode: TJclTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AObject, nil);

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AObject)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclTreeNode.Create;
            NewNode.Value := AObject;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclTreeNode.Create;
        FRoot.Value := AObject;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree.AddAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclTree;
  ACollection: IJclCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclTree then
  begin
    ADest := TJclTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclTree then
    TJclTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      RemoveNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree.CloneNode(Node, Parent: TJclTreeNode): TJclTreeNode;
var
  Index: Integer;
begin
  Result := TJclTreeNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  SetLength(Result.Children, Node.ChildrenCount);
  Result.ChildrenCount := Node.ChildrenCount;
  for Index := 0 to Node.ChildrenCount - 1 do
    Result.Children[Index] := CloneNode(TJclTreeNode(Node.Children[Index]), Result); // recursive call
end;

function TJclTree.CollectionEquals(const ACollection: IJclCollection): Boolean;
var
  It, ItSelf: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree.Contains(AObject: TObject): Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AObject)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree.ContainsAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree.Extract(AObject: TObject): Boolean;
var
  It: IJclIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AObject) then
      begin
        It.Extract;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree.ExtractAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTree.ExtractNode(var ANode: TJclTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ExtractNode(TJclTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ExtractNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclTree.First: IJclIterator;
var
  Start: TJclTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderTreeIterator.Create(Self, Start, False, isFirst);
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclTreeNode(Start.Children[0]);
          Result := TJclPostOrderTreeIterator.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclTree.GetEnumerator: IJclIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclTree.GetRoot: IJclTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderTreeIterator.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclTree.Last: IJclIterator;
var
  Start: TJclTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while Start.ChildrenCount > 0 do
              Start := TJclTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TJclPreOrderTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderTreeIterator.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree.NodeContains(ANode: TJclTreeNode; AObject: TObject): Boolean;
var
  Index: Integer;
begin
  Result := ItemsEqual(ANode.Value, AObject);
  if not Result then
    for Index := 0 to ANode.ChildrenCount - 1 do
  begin
    Result := NodeContains(TJclTreeNode(ANode.Children[Index]), AObject);
    if Result then
      Break;
  end;
end;

procedure TJclTree.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTree.PackNode(ANode: TJclTreeNode);
var
  Index: Integer;
begin
  SetLength(ANode.Children, ANode.ChildrenCount);
  for Index := 0 to ANode.ChildrenCount - 1 do
    PackNode(TJclTreeNode(ANode.Children[Index]));
end;

function TJclTree.Remove(AObject: TObject): Boolean;
var
  Extracted: TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AObject);
    if Result then
    begin
      Extracted := AObject;
      FreeObject(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree.RemoveAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTree.RemoveNode(var ANode: TJclTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    RemoveNode(TJclTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    RemoveNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeObject(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclTree.RetainAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclTree.Create(False);
  AssignPropertiesTo(Result);
end;

//=== { TJclTreeIterator } ===========================================================

constructor TJclTreeIterator.Create(OwnTree: TJclTree; ACursor: TJclTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclEqualityComparer;
end;

function TJclTreeIterator.Add(AObject: TObject): Boolean;
var
  ParentNode, NewNode: TJclTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AObject, nil))
      and ((not FOwnTree.Contains(AObject)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclTreeNode.Create;
        NewNode.Value := AObject;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator.AddChild(AObject: TObject): Boolean;
var
  NewNode: TJclTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AObject, nil))
      and ((not FOwnTree.Contains(AObject)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclTreeNode.Create;
        NewNode.Value := AObject;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclTreeIterator then
  begin
    ADest := TJclTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTreeIterator.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.RemoveNode(TJclTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.RemoveNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTreeIterator.DeleteChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.RemoveNode(TJclTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.RemoveNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTreeIterator.Extract;
var
  OldCursor: TJclTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ExtractNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTreeIterator.ExtractChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ExtractNode(TJclTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ExtractNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTreeIterator.ExtractChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ExtractNode(TJclTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ExtractNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator.GetChild(Index: Integer): TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclTreeNode(FCursor.Children[Index]);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator.GetObject: TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator.IndexOfChild(AObject: TObject): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AObject, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator.Insert(AObject: TObject): Boolean;
var
  ParentNode, NewNode: TJclTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AObject, nil))
      and ((not FOwnTree.Contains(AObject)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclTreeNode.Create;
        NewNode.Value := AObject;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator.InsertChild(Index: Integer; AObject: TObject): Boolean;
var
  NewNode: TJclTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AObject, nil))
      and ((not FOwnTree.Contains(AObject)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclTreeNode.Create;
        NewNode.Value := AObject;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator.IteratorEquals(const AIterator: IJclIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclTreeIterator then
  begin
    ItrObj := TJclTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclTreeIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclTreeIterator.Next: TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclTreeIterator.Parent: TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator.Previous: TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclTreeIterator.Remove;
var
  OldCursor: TJclTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.RemoveNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTreeIterator.Reset;
var
  NewCursor: TJclTreeNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTreeIterator.SetChild(Index: Integer; AObject: TObject);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclTreeNode(FCursor.Children[Index]).Value := AObject
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTreeIterator.SetObject(AObject: TObject);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeObject(FCursor.Value);
      FCursor.Value := AObject;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclPreOrderTreeIterator } ===================================================

function TJclPreOrderTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderTreeIterator.GetNextCursor: TJclTreeNode;
var
  LastRet: TJclTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TJclPreOrderTreeIterator.GetNextSibling: TJclTreeNode;
var
  LastRet: TJclTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TJclPreOrderTreeIterator.GetPreviousCursor: TJclTreeNode;
var
  LastRet: TJclTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TJclPostOrderTreeIterator } ==================================================

function TJclPostOrderTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderTreeIterator.GetNextCursor: TJclTreeNode;
var
  LastRet: TJclTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderTreeIterator.GetNextSibling: TJclTreeNode;
var
  LastRet: TJclTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclTreeNode(Result.Children[0]);
  end;
end;

function TJclPostOrderTreeIterator.GetPreviousCursor: TJclTreeNode;
var
  LastRet: TJclTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;


{$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

//=== { TJclTreeNode<T> } =======================================================

function TJclTreeNode<T>.IndexOfChild(AChild: TJclTreeNode<T>): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclTreeNode<T>.IndexOfValue(const AItem: T;
  const AEqualityComparer: IJclEqualityComparer<T>): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclTreeNode<T>(Children[Result]).Value, AItem) then
      Exit;
  Result := -1;
end;

//=== { TJclTree<T> } =======================================================

constructor TJclTree<T>.Create(AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FTraverseOrder := toPreOrder;
end;

destructor TJclTree<T>.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclTree<T>.Add(const AItem: T): Boolean;
var
  NewNode: TTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AItem, Default(T));

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AItem)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TTreeNode.Create;
            NewNode.Value := AItem;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TTreeNode.Create;
        FRoot.Value := AItem;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTree<T>.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclTree<T>;
  ACollection: IJclCollection<T>;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclTree<T> then
  begin
    ADest := TJclTree<T>(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclCollection<T>, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclTree<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclTree<T> then
    TJclTree<T>(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclTree<T>.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      RemoveNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree<T>.CloneNode(Node, Parent: TTreeNode): TTreeNode;
var
  Index: Integer;
begin
  Result := TTreeNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  SetLength(Result.Children, Node.ChildrenCount);
  Result.ChildrenCount := Node.ChildrenCount;
  for Index := 0 to Node.ChildrenCount - 1 do
    Result.Children[Index] := CloneNode(TTreeNode(Node.Children[Index]), Result); // recursive call
end;

function TJclTree<T>.CollectionEquals(const ACollection: IJclCollection<T>): Boolean;
var
  It, ItSelf: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree<T>.Contains(const AItem: T): Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AItem)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree<T>.ContainsAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree<T>.Extract(const AItem: T): Boolean;
var
  It: IJclIterator<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AItem) then
      begin
        It.Extract;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree<T>.ExtractAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTree<T>.ExtractNode(var ANode: TTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ExtractNode(TTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ExtractNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclTree<T>.First: IJclIterator<T>;
var
  Start: TTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderTreeIterator.Create(Self, Start, False, isFirst);
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TTreeNode(Start.Children[0]);
          Result := TPostOrderTreeIterator.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclTree<T>.GetEnumerator: IJclIterator<T>;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclTree<T>.GetRoot: IJclTreeIterator<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TPostOrderTreeIterator.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree<T>.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclTree<T>.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclTree<T>.Last: IJclIterator<T>;
var
  Start: TTreeNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while Start.ChildrenCount > 0 do
              Start := TTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TPreOrderTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TPostOrderTreeIterator.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree<T>.NodeContains(ANode: TTreeNode; const AItem: T): Boolean;
var
  Index: Integer;
begin
  Result := ItemsEqual(ANode.Value, AItem);
  if not Result then
    for Index := 0 to ANode.ChildrenCount - 1 do
  begin
    Result := NodeContains(TTreeNode(ANode.Children[Index]), AItem);
    if Result then
      Break;
  end;
end;

procedure TJclTree<T>.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTree<T>.PackNode(ANode: TTreeNode);
var
  Index: Integer;
begin
  SetLength(ANode.Children, ANode.ChildrenCount);
  for Index := 0 to ANode.ChildrenCount - 1 do
    PackNode(TTreeNode(ANode.Children[Index]));
end;

function TJclTree<T>.Remove(const AItem: T): Boolean;
var
  Extracted: T;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AItem);
    if Result then
    begin
      Extracted := AItem;
      FreeItem(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree<T>.RemoveAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTree<T>.RemoveNode(var ANode: TTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    RemoveNode(TTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    RemoveNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeItem(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclTree<T>.RetainAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTree<T>.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclTree<T>.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclTree<T>.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclTreeIterator<T> } ===========================================================

constructor TJclTreeIterator<T>.Create(OwnTree: TJclTree<T>; ACursor: TJclTreeNode<T>; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclEqualityComparer<T>;
end;

function TJclTreeIterator<T>.Add(const AItem: T): Boolean;
var
  ParentNode, NewNode: TJclTreeNode<T>;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclTree<T>.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AItem, Default(T)))
      and ((not FOwnTree.Contains(AItem)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclTreeNode<T>.Create;
        NewNode.Value := AItem;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator<T>.AddChild(const AItem: T): Boolean;
var
  NewNode: TJclTreeNode<T>;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AItem, Default(T)))
      and ((not FOwnTree.Contains(AItem)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclTreeNode<T>.Create;
        NewNode.Value := AItem;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTreeIterator<T>.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclTreeIterator<T>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclTreeIterator<T> then
  begin
    ADest := TJclTreeIterator<T>(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclTreeIterator<T>.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTreeIterator<T>.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.RemoveNode(TJclTreeNode<T>(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.RemoveNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTreeIterator<T>.DeleteChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.RemoveNode(TJclTreeNode<T>(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.RemoveNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTreeIterator<T>.Extract;
var
  OldCursor: TJclTreeNode<T>;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ExtractNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTreeIterator<T>.ExtractChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ExtractNode(TJclTreeNode<T>(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ExtractNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTreeIterator<T>.ExtractChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ExtractNode(TJclTreeNode<T>(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ExtractNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator<T>.GetChild(Index: Integer): T;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclTreeNode<T>(FCursor.Children[Index]);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator<T>.GetItem: T;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := Default(T);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator<T>.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator<T>.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator<T>.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator<T>.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator<T>.IndexOfChild(const AItem: T): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AItem, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator<T>.Insert(const AItem: T): Boolean;
var
  ParentNode, NewNode: TJclTreeNode<T>;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclTree<T>.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AItem, Default(T)))
      and ((not FOwnTree.Contains(AItem)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclTreeNode<T>.Create;
        NewNode.Value := AItem;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator<T>.InsertChild(Index: Integer; const AItem: T): Boolean;
var
  NewNode: TJclTreeNode<T>;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclTree<T>.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AItem, Default(T)))
      and ((not FOwnTree.Contains(AItem)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclTreeNode<T>.Create;
        NewNode.Value := AItem;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator<T>.IteratorEquals(const AIterator: IJclIterator<T>): Boolean;
var
  Obj: TObject;
  ItrObj: TJclTreeIterator<T>;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclTreeIterator<T> then
  begin
    ItrObj := TJclTreeIterator<T>(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclTreeIterator<T>.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclTreeIterator<T>.Next: T;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := Default(T);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator<T>.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclTreeIterator<T>.Parent: T;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator<T>.Previous: T;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := Default(T);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTreeIterator<T>.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclTreeIterator<T>.Remove;
var
  OldCursor: TJclTreeNode<T>;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.RemoveNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTreeIterator<T>.Reset;
var
  NewCursor: TJclTreeNode<T>;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTreeIterator<T>.SetChild(Index: Integer; const AItem: T);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclTreeNode<T>(FCursor.Children[Index]).Value := AItem
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTreeIterator<T>.SetItem(const AItem: T);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeItem(FCursor.Value);
      FCursor.Value := AItem;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclPreOrderTreeIterator<T> } ===================================================

function TJclPreOrderTreeIterator<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderTreeIterator<T>.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderTreeIterator<T>.GetNextCursor: TJclTreeNode<T>;
var
  LastRet: TJclTreeNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclTreeNode<T>(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclTreeNode<T>(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TJclPreOrderTreeIterator<T>.GetNextSibling: TJclTreeNode<T>;
var
  LastRet: TJclTreeNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclTreeNode<T>(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TJclPreOrderTreeIterator<T>.GetPreviousCursor: TJclTreeNode<T>;
var
  LastRet: TJclTreeNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclTreeNode<T>(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclTreeNode<T>(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TJclPostOrderTreeIterator<T> } ==================================================

function TJclPostOrderTreeIterator<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderTreeIterator<T>.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderTreeIterator<T>.GetNextCursor: TJclTreeNode<T>;
var
  LastRet: TJclTreeNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclTreeNode<T>(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclTreeNode<T>(Result.Children[0]);
  end;
end;

function TJclPostOrderTreeIterator<T>.GetNextSibling: TJclTreeNode<T>;
var
  LastRet: TJclTreeNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclTreeNode<T>(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclTreeNode<T>(Result.Children[0]);
  end;
end;

function TJclPostOrderTreeIterator<T>.GetPreviousCursor: TJclTreeNode<T>;
var
  LastRet: TJclTreeNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclTreeNode<T>(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclTreeNode<T>(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

//=== { TJclTreeE<T> } =======================================================

constructor TJclTreeE<T>.Create(const AEqualityComparer: IJclEqualityComparer<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

procedure TJclTreeE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclTreeE<T> then
    TJclTreeE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclTreeE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclTreeE<T>.Create(EqualityComparer, False);
  AssignPropertiesTo(Result);
end;

function TJclTreeE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer <> nil then
    Result := EqualityComparer.ItemsEqual(A, B)
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclTreeF<T> } =======================================================

constructor TJclTreeF<T>.Create(ACompare: TCompare<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  SetCompare(ACompare);
end;

function TJclTreeF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclTreeF<T>.Create(Compare, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclTreeI<T> } =======================================================

function TJclTreeI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclTreeI<T>.Create(False);
  AssignPropertiesTo(Result);
end;

function TJclTreeI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
    Result := A.Equals(B);
end;

//DOM-IGNORE-END
{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
