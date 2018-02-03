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
{ The Original Code is BinaryTree.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Florent Ouchet (outchy)                                                                        }
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

unit JclBinaryTrees;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase, JclAbstractContainers, JclAlgorithms, JclContainerIntf, JclSynch;

type
  TItrStart = (isFirst, isLast, isRoot);

  TJclIntfBinaryNode = class
  public
    Value: IInterface;
    Left: TJclIntfBinaryNode;
    Right: TJclIntfBinaryNode;
    Parent: TJclIntfBinaryNode;
  end;

  TJclIntfBinaryTree = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclBaseContainer,
    IJclIntfEqualityComparer, IJclIntfComparer, IJclIntfContainer, IJclIntfFlatContainer,
    IJclIntfCollection, IJclIntfTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FMaxDepth: Integer;
    FRoot: TJclIntfBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    function BuildTree(const LeafArray: array of TJclIntfBinaryNode; Left, Right: Integer; Parent: TJclIntfBinaryNode;
      Offset: Integer): TJclIntfBinaryNode;
    function CloneNode(Node, Parent: TJclIntfBinaryNode): TJclIntfBinaryNode;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
  public
    constructor Create(ACompare: TIntfCompare);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCollection }
    function Add(const AInterface: IInterface): Boolean;
    function AddAll(const ACollection: IJclIntfCollection): Boolean;
    procedure Clear;
    function Contains(const AInterface: IInterface): Boolean;
    function ContainsAll(const ACollection: IJclIntfCollection): Boolean;
    function CollectionEquals(const ACollection: IJclIntfCollection): Boolean;
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

  TJclIntfBinaryTreeIterator = class(TJclAbstractIterator, IJclIntfIterator, IJclIntfTreeIterator, IJclIntfBinaryTreeIterator)
  protected
    FCursor: TJclIntfBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclIntfCollection;
    FEqualityComparer: IJclIntfEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclIntfBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclIntfBinaryNode; virtual; abstract;
  public
    constructor Create(const AOwnTree: IJclIntfCollection; ACursor: TJclIntfBinaryNode; AValid: Boolean; AStart: TItrStart);
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
    { IJclIntfBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: IInterface;
    function Right: IInterface;
  end;

  TJclPreOrderIntfBinaryTreeIterator = class(TJclIntfBinaryTreeIterator, IJclIntfIterator, IJclIntfTreeIterator, IJclIntfBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntfBinaryNode; override;
    function GetPreviousCursor: TJclIntfBinaryNode; override;
  end;

  TJclInOrderIntfBinaryTreeIterator = class(TJclIntfBinaryTreeIterator, IJclIntfIterator, IJclIntfTreeIterator, IJclIntfBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntfBinaryNode; override;
    function GetPreviousCursor: TJclIntfBinaryNode; override;
  end;

  TJclPostOrderIntfBinaryTreeIterator = class(TJclIntfBinaryTreeIterator, IJclIntfIterator, IJclIntfTreeIterator, IJclIntfBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntfBinaryNode; override;
    function GetPreviousCursor: TJclIntfBinaryNode; override;
  end;

  TJclAnsiStrBinaryNode = class
  public
    Value: AnsiString;
    Left: TJclAnsiStrBinaryNode;
    Right: TJclAnsiStrBinaryNode;
    Parent: TJclAnsiStrBinaryNode;
  end;

  TJclAnsiStrBinaryTree = class(TJclAnsiStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclBaseContainer,
    IJclAnsiStrEqualityComparer, IJclAnsiStrComparer, IJclAnsiStrContainer, IJclAnsiStrFlatContainer, IJclStrBaseContainer,
    IJclAnsiStrCollection, IJclAnsiStrTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FMaxDepth: Integer;
    FRoot: TJclAnsiStrBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    function BuildTree(const LeafArray: array of TJclAnsiStrBinaryNode; Left, Right: Integer; Parent: TJclAnsiStrBinaryNode;
      Offset: Integer): TJclAnsiStrBinaryNode;
    function CloneNode(Node, Parent: TJclAnsiStrBinaryNode): TJclAnsiStrBinaryNode;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
  public
    constructor Create(ACompare: TAnsiStrCompare);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclAnsiStrCollection }
    function Add(const AString: AnsiString): Boolean; override;
    function AddAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    procedure Clear; override;
    function Contains(const AString: AnsiString): Boolean; override;
    function ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function CollectionEquals(const ACollection: IJclAnsiStrCollection): Boolean; override;
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

  TJclAnsiStrBinaryTreeIterator = class(TJclAbstractIterator, IJclAnsiStrIterator, IJclAnsiStrTreeIterator, IJclAnsiStrBinaryTreeIterator)
  protected
    FCursor: TJclAnsiStrBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclAnsiStrCollection;
    FEqualityComparer: IJclAnsiStrEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclAnsiStrBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclAnsiStrBinaryNode; virtual; abstract;
  public
    constructor Create(const AOwnTree: IJclAnsiStrCollection; ACursor: TJclAnsiStrBinaryNode; AValid: Boolean; AStart: TItrStart);
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
    { IJclAnsiStrBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: AnsiString;
    function Right: AnsiString;
  end;

  TJclPreOrderAnsiStrBinaryTreeIterator = class(TJclAnsiStrBinaryTreeIterator, IJclAnsiStrIterator, IJclAnsiStrTreeIterator, IJclAnsiStrBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclAnsiStrBinaryNode; override;
    function GetPreviousCursor: TJclAnsiStrBinaryNode; override;
  end;

  TJclInOrderAnsiStrBinaryTreeIterator = class(TJclAnsiStrBinaryTreeIterator, IJclAnsiStrIterator, IJclAnsiStrTreeIterator, IJclAnsiStrBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclAnsiStrBinaryNode; override;
    function GetPreviousCursor: TJclAnsiStrBinaryNode; override;
  end;

  TJclPostOrderAnsiStrBinaryTreeIterator = class(TJclAnsiStrBinaryTreeIterator, IJclAnsiStrIterator, IJclAnsiStrTreeIterator, IJclAnsiStrBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclAnsiStrBinaryNode; override;
    function GetPreviousCursor: TJclAnsiStrBinaryNode; override;
  end;

  TJclWideStrBinaryNode = class
  public
    Value: WideString;
    Left: TJclWideStrBinaryNode;
    Right: TJclWideStrBinaryNode;
    Parent: TJclWideStrBinaryNode;
  end;

  TJclWideStrBinaryTree = class(TJclWideStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclBaseContainer,
    IJclWideStrEqualityComparer, IJclWideStrComparer, IJclWideStrContainer, IJclWideStrFlatContainer, IJclStrBaseContainer,
    IJclWideStrCollection, IJclWideStrTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FMaxDepth: Integer;
    FRoot: TJclWideStrBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    function BuildTree(const LeafArray: array of TJclWideStrBinaryNode; Left, Right: Integer; Parent: TJclWideStrBinaryNode;
      Offset: Integer): TJclWideStrBinaryNode;
    function CloneNode(Node, Parent: TJclWideStrBinaryNode): TJclWideStrBinaryNode;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
  public
    constructor Create(ACompare: TWideStrCompare);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclWideStrCollection }
    function Add(const AString: WideString): Boolean; override;
    function AddAll(const ACollection: IJclWideStrCollection): Boolean; override;
    procedure Clear; override;
    function Contains(const AString: WideString): Boolean; override;
    function ContainsAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function CollectionEquals(const ACollection: IJclWideStrCollection): Boolean; override;
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

  TJclWideStrBinaryTreeIterator = class(TJclAbstractIterator, IJclWideStrIterator, IJclWideStrTreeIterator, IJclWideStrBinaryTreeIterator)
  protected
    FCursor: TJclWideStrBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclWideStrCollection;
    FEqualityComparer: IJclWideStrEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclWideStrBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclWideStrBinaryNode; virtual; abstract;
  public
    constructor Create(const AOwnTree: IJclWideStrCollection; ACursor: TJclWideStrBinaryNode; AValid: Boolean; AStart: TItrStart);
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
    { IJclWideStrBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: WideString;
    function Right: WideString;
  end;

  TJclPreOrderWideStrBinaryTreeIterator = class(TJclWideStrBinaryTreeIterator, IJclWideStrIterator, IJclWideStrTreeIterator, IJclWideStrBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclWideStrBinaryNode; override;
    function GetPreviousCursor: TJclWideStrBinaryNode; override;
  end;

  TJclInOrderWideStrBinaryTreeIterator = class(TJclWideStrBinaryTreeIterator, IJclWideStrIterator, IJclWideStrTreeIterator, IJclWideStrBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclWideStrBinaryNode; override;
    function GetPreviousCursor: TJclWideStrBinaryNode; override;
  end;

  TJclPostOrderWideStrBinaryTreeIterator = class(TJclWideStrBinaryTreeIterator, IJclWideStrIterator, IJclWideStrTreeIterator, IJclWideStrBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclWideStrBinaryNode; override;
    function GetPreviousCursor: TJclWideStrBinaryNode; override;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrBinaryNode = class
  public
    Value: UnicodeString;
    Left: TJclUnicodeStrBinaryNode;
    Right: TJclUnicodeStrBinaryNode;
    Parent: TJclUnicodeStrBinaryNode;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrBinaryTree = class(TJclUnicodeStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclBaseContainer,
    IJclUnicodeStrEqualityComparer, IJclUnicodeStrComparer, IJclUnicodeStrContainer, IJclUnicodeStrFlatContainer, IJclStrBaseContainer,
    IJclUnicodeStrCollection, IJclUnicodeStrTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FMaxDepth: Integer;
    FRoot: TJclUnicodeStrBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    function BuildTree(const LeafArray: array of TJclUnicodeStrBinaryNode; Left, Right: Integer; Parent: TJclUnicodeStrBinaryNode;
      Offset: Integer): TJclUnicodeStrBinaryNode;
    function CloneNode(Node, Parent: TJclUnicodeStrBinaryNode): TJclUnicodeStrBinaryNode;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
  public
    constructor Create(ACompare: TUnicodeStrCompare);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclUnicodeStrCollection }
    function Add(const AString: UnicodeString): Boolean; override;
    function AddAll(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    procedure Clear; override;
    function Contains(const AString: UnicodeString): Boolean; override;
    function ContainsAll(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    function CollectionEquals(const ACollection: IJclUnicodeStrCollection): Boolean; override;
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
  TJclUnicodeStrBinaryTreeIterator = class(TJclAbstractIterator, IJclUnicodeStrIterator, IJclUnicodeStrTreeIterator, IJclUnicodeStrBinaryTreeIterator)
  protected
    FCursor: TJclUnicodeStrBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclUnicodeStrCollection;
    FEqualityComparer: IJclUnicodeStrEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclUnicodeStrBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclUnicodeStrBinaryNode; virtual; abstract;
  public
    constructor Create(const AOwnTree: IJclUnicodeStrCollection; ACursor: TJclUnicodeStrBinaryNode; AValid: Boolean; AStart: TItrStart);
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
    { IJclUnicodeStrBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: UnicodeString;
    function Right: UnicodeString;
  end;

  TJclPreOrderUnicodeStrBinaryTreeIterator = class(TJclUnicodeStrBinaryTreeIterator, IJclUnicodeStrIterator, IJclUnicodeStrTreeIterator, IJclUnicodeStrBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclUnicodeStrBinaryNode; override;
    function GetPreviousCursor: TJclUnicodeStrBinaryNode; override;
  end;

  TJclInOrderUnicodeStrBinaryTreeIterator = class(TJclUnicodeStrBinaryTreeIterator, IJclUnicodeStrIterator, IJclUnicodeStrTreeIterator, IJclUnicodeStrBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclUnicodeStrBinaryNode; override;
    function GetPreviousCursor: TJclUnicodeStrBinaryNode; override;
  end;

  TJclPostOrderUnicodeStrBinaryTreeIterator = class(TJclUnicodeStrBinaryTreeIterator, IJclUnicodeStrIterator, IJclUnicodeStrTreeIterator, IJclUnicodeStrBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclUnicodeStrBinaryNode; override;
    function GetPreviousCursor: TJclUnicodeStrBinaryNode; override;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrBinaryNode = TJclAnsiStrBinaryNode;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrBinaryNode = TJclWideStrBinaryNode;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrBinaryNode = TJclUnicodeStrBinaryNode;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrBinaryTree = TJclAnsiStrBinaryTree;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrBinaryTree = TJclWideStrBinaryTree;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrBinaryTree = TJclUnicodeStrBinaryTree;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrBinaryTreeIterator = TJclAnsiStrBinaryTreeIterator;
  TJclPreOrderStrBinaryTreeIterator = TJclPreOrderAnsiStrBinaryTreeIterator;
  TJclInOrderStrBinaryTreeIterator = TJclInOrderAnsiStrBinaryTreeIterator;
  TJclPostOrderStrBinaryTreeIterator = TJclPostOrderAnsiStrBinaryTreeIterator;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrBinaryTreeIterator = TJclWideStrBinaryTreeIterator;
  TJclPreOrderStrBinaryTreeIterator = TJclPreOrderWideStrBinaryTreeIterator;
  TJclInOrderStrBinaryTreeIterator = TJclInOrderWideStrBinaryTreeIterator;
  TJclPostOrderStrBinaryTreeIterator = TJclPostOrderWideStrBinaryTreeIterator;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrBinaryTreeIterator = TJclUnicodeStrBinaryTreeIterator;
  TJclPreOrderStrBinaryTreeIterator = TJclPreOrderUnicodeStrBinaryTreeIterator;
  TJclInOrderStrBinaryTreeIterator = TJclInOrderUnicodeStrBinaryTreeIterator;
  TJclPostOrderStrBinaryTreeIterator = TJclPostOrderUnicodeStrBinaryTreeIterator;
  {$ENDIF CONTAINER_UNICODESTR}

  TJclSingleBinaryNode = class
  public
    Value: Single;
    Left: TJclSingleBinaryNode;
    Right: TJclSingleBinaryNode;
    Parent: TJclSingleBinaryNode;
  end;

  TJclSingleBinaryTree = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclBaseContainer,
    IJclSingleEqualityComparer, IJclSingleComparer, IJclSingleContainer, IJclSingleFlatContainer,
    IJclSingleCollection, IJclSingleTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FMaxDepth: Integer;
    FRoot: TJclSingleBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    function BuildTree(const LeafArray: array of TJclSingleBinaryNode; Left, Right: Integer; Parent: TJclSingleBinaryNode;
      Offset: Integer): TJclSingleBinaryNode;
    function CloneNode(Node, Parent: TJclSingleBinaryNode): TJclSingleBinaryNode;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
  public
    constructor Create(ACompare: TSingleCompare);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclSingleCollection }
    function Add(const AValue: Single): Boolean;
    function AddAll(const ACollection: IJclSingleCollection): Boolean;
    procedure Clear;
    function Contains(const AValue: Single): Boolean;
    function ContainsAll(const ACollection: IJclSingleCollection): Boolean;
    function CollectionEquals(const ACollection: IJclSingleCollection): Boolean;
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

  TJclSingleBinaryTreeIterator = class(TJclAbstractIterator, IJclSingleIterator, IJclSingleTreeIterator, IJclSingleBinaryTreeIterator)
  protected
    FCursor: TJclSingleBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclSingleCollection;
    FEqualityComparer: IJclSingleEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclSingleBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclSingleBinaryNode; virtual; abstract;
  public
    constructor Create(const AOwnTree: IJclSingleCollection; ACursor: TJclSingleBinaryNode; AValid: Boolean; AStart: TItrStart);
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
    { IJclSingleBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Single;
    function Right: Single;
  end;

  TJclPreOrderSingleBinaryTreeIterator = class(TJclSingleBinaryTreeIterator, IJclSingleIterator, IJclSingleTreeIterator, IJclSingleBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclSingleBinaryNode; override;
    function GetPreviousCursor: TJclSingleBinaryNode; override;
  end;

  TJclInOrderSingleBinaryTreeIterator = class(TJclSingleBinaryTreeIterator, IJclSingleIterator, IJclSingleTreeIterator, IJclSingleBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclSingleBinaryNode; override;
    function GetPreviousCursor: TJclSingleBinaryNode; override;
  end;

  TJclPostOrderSingleBinaryTreeIterator = class(TJclSingleBinaryTreeIterator, IJclSingleIterator, IJclSingleTreeIterator, IJclSingleBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclSingleBinaryNode; override;
    function GetPreviousCursor: TJclSingleBinaryNode; override;
  end;

  TJclDoubleBinaryNode = class
  public
    Value: Double;
    Left: TJclDoubleBinaryNode;
    Right: TJclDoubleBinaryNode;
    Parent: TJclDoubleBinaryNode;
  end;

  TJclDoubleBinaryTree = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclBaseContainer,
    IJclDoubleEqualityComparer, IJclDoubleComparer, IJclDoubleContainer, IJclDoubleFlatContainer,
    IJclDoubleCollection, IJclDoubleTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FMaxDepth: Integer;
    FRoot: TJclDoubleBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    function BuildTree(const LeafArray: array of TJclDoubleBinaryNode; Left, Right: Integer; Parent: TJclDoubleBinaryNode;
      Offset: Integer): TJclDoubleBinaryNode;
    function CloneNode(Node, Parent: TJclDoubleBinaryNode): TJclDoubleBinaryNode;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
  public
    constructor Create(ACompare: TDoubleCompare);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclDoubleCollection }
    function Add(const AValue: Double): Boolean;
    function AddAll(const ACollection: IJclDoubleCollection): Boolean;
    procedure Clear;
    function Contains(const AValue: Double): Boolean;
    function ContainsAll(const ACollection: IJclDoubleCollection): Boolean;
    function CollectionEquals(const ACollection: IJclDoubleCollection): Boolean;
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

  TJclDoubleBinaryTreeIterator = class(TJclAbstractIterator, IJclDoubleIterator, IJclDoubleTreeIterator, IJclDoubleBinaryTreeIterator)
  protected
    FCursor: TJclDoubleBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclDoubleCollection;
    FEqualityComparer: IJclDoubleEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclDoubleBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclDoubleBinaryNode; virtual; abstract;
  public
    constructor Create(const AOwnTree: IJclDoubleCollection; ACursor: TJclDoubleBinaryNode; AValid: Boolean; AStart: TItrStart);
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
    { IJclDoubleBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Double;
    function Right: Double;
  end;

  TJclPreOrderDoubleBinaryTreeIterator = class(TJclDoubleBinaryTreeIterator, IJclDoubleIterator, IJclDoubleTreeIterator, IJclDoubleBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclDoubleBinaryNode; override;
    function GetPreviousCursor: TJclDoubleBinaryNode; override;
  end;

  TJclInOrderDoubleBinaryTreeIterator = class(TJclDoubleBinaryTreeIterator, IJclDoubleIterator, IJclDoubleTreeIterator, IJclDoubleBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclDoubleBinaryNode; override;
    function GetPreviousCursor: TJclDoubleBinaryNode; override;
  end;

  TJclPostOrderDoubleBinaryTreeIterator = class(TJclDoubleBinaryTreeIterator, IJclDoubleIterator, IJclDoubleTreeIterator, IJclDoubleBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclDoubleBinaryNode; override;
    function GetPreviousCursor: TJclDoubleBinaryNode; override;
  end;

  TJclExtendedBinaryNode = class
  public
    Value: Extended;
    Left: TJclExtendedBinaryNode;
    Right: TJclExtendedBinaryNode;
    Parent: TJclExtendedBinaryNode;
  end;

  TJclExtendedBinaryTree = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclBaseContainer,
    IJclExtendedEqualityComparer, IJclExtendedComparer, IJclExtendedContainer, IJclExtendedFlatContainer,
    IJclExtendedCollection, IJclExtendedTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FMaxDepth: Integer;
    FRoot: TJclExtendedBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    function BuildTree(const LeafArray: array of TJclExtendedBinaryNode; Left, Right: Integer; Parent: TJclExtendedBinaryNode;
      Offset: Integer): TJclExtendedBinaryNode;
    function CloneNode(Node, Parent: TJclExtendedBinaryNode): TJclExtendedBinaryNode;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
  public
    constructor Create(ACompare: TExtendedCompare);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclExtendedCollection }
    function Add(const AValue: Extended): Boolean;
    function AddAll(const ACollection: IJclExtendedCollection): Boolean;
    procedure Clear;
    function Contains(const AValue: Extended): Boolean;
    function ContainsAll(const ACollection: IJclExtendedCollection): Boolean;
    function CollectionEquals(const ACollection: IJclExtendedCollection): Boolean;
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

  TJclExtendedBinaryTreeIterator = class(TJclAbstractIterator, IJclExtendedIterator, IJclExtendedTreeIterator, IJclExtendedBinaryTreeIterator)
  protected
    FCursor: TJclExtendedBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclExtendedCollection;
    FEqualityComparer: IJclExtendedEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclExtendedBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclExtendedBinaryNode; virtual; abstract;
  public
    constructor Create(const AOwnTree: IJclExtendedCollection; ACursor: TJclExtendedBinaryNode; AValid: Boolean; AStart: TItrStart);
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
    { IJclExtendedBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Extended;
    function Right: Extended;
  end;

  TJclPreOrderExtendedBinaryTreeIterator = class(TJclExtendedBinaryTreeIterator, IJclExtendedIterator, IJclExtendedTreeIterator, IJclExtendedBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclExtendedBinaryNode; override;
    function GetPreviousCursor: TJclExtendedBinaryNode; override;
  end;

  TJclInOrderExtendedBinaryTreeIterator = class(TJclExtendedBinaryTreeIterator, IJclExtendedIterator, IJclExtendedTreeIterator, IJclExtendedBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclExtendedBinaryNode; override;
    function GetPreviousCursor: TJclExtendedBinaryNode; override;
  end;

  TJclPostOrderExtendedBinaryTreeIterator = class(TJclExtendedBinaryTreeIterator, IJclExtendedIterator, IJclExtendedTreeIterator, IJclExtendedBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclExtendedBinaryNode; override;
    function GetPreviousCursor: TJclExtendedBinaryNode; override;
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatBinaryNode = TJclSingleBinaryNode;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatBinaryNode = TJclDoubleBinaryNode;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatBinaryNode = TJclExtendedBinaryNode;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatBinaryTree = TJclSingleBinaryTree;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatBinaryTree = TJclDoubleBinaryTree;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatBinaryTree = TJclExtendedBinaryTree;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatBinaryTreeIterator = TJclSingleBinaryTreeIterator;
  TJclPreOrderFloatBinaryTreeIterator = TJclPreOrderSingleBinaryTreeIterator;
  TJclInOrderFloatBinaryTreeIterator = TJclInOrderSingleBinaryTreeIterator;
  TJclPostOrderFloatBinaryTreeIterator = TJclPostOrderSingleBinaryTreeIterator;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatBinaryTreeIterator = TJclDoubleBinaryTreeIterator;
  TJclPreOrderFloatBinaryTreeIterator = TJclPreOrderDoubleBinaryTreeIterator;
  TJclInOrderFloatBinaryTreeIterator = TJclInOrderDoubleBinaryTreeIterator;
  TJclPostOrderFloatBinaryTreeIterator = TJclPostOrderDoubleBinaryTreeIterator;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatBinaryTreeIterator = TJclExtendedBinaryTreeIterator;
  TJclPreOrderFloatBinaryTreeIterator = TJclPreOrderExtendedBinaryTreeIterator;
  TJclInOrderFloatBinaryTreeIterator = TJclInOrderExtendedBinaryTreeIterator;
  TJclPostOrderFloatBinaryTreeIterator = TJclPostOrderExtendedBinaryTreeIterator;
  {$ENDIF MATH_EXTENDED_PRECISION}

  TJclIntegerBinaryNode = class
  public
    Value: Integer;
    Left: TJclIntegerBinaryNode;
    Right: TJclIntegerBinaryNode;
    Parent: TJclIntegerBinaryNode;
  end;

  TJclIntegerBinaryTree = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclBaseContainer,
    IJclIntegerEqualityComparer, IJclIntegerComparer, IJclIntegerContainer, IJclIntegerFlatContainer,
    IJclIntegerCollection, IJclIntegerTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FMaxDepth: Integer;
    FRoot: TJclIntegerBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    function BuildTree(const LeafArray: array of TJclIntegerBinaryNode; Left, Right: Integer; Parent: TJclIntegerBinaryNode;
      Offset: Integer): TJclIntegerBinaryNode;
    function CloneNode(Node, Parent: TJclIntegerBinaryNode): TJclIntegerBinaryNode;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
  public
    constructor Create(ACompare: TIntegerCompare);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntegerCollection }
    function Add(AValue: Integer): Boolean;
    function AddAll(const ACollection: IJclIntegerCollection): Boolean;
    procedure Clear;
    function Contains(AValue: Integer): Boolean;
    function ContainsAll(const ACollection: IJclIntegerCollection): Boolean;
    function CollectionEquals(const ACollection: IJclIntegerCollection): Boolean;
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

  TJclIntegerBinaryTreeIterator = class(TJclAbstractIterator, IJclIntegerIterator, IJclIntegerTreeIterator, IJclIntegerBinaryTreeIterator)
  protected
    FCursor: TJclIntegerBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclIntegerCollection;
    FEqualityComparer: IJclIntegerEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclIntegerBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclIntegerBinaryNode; virtual; abstract;
  public
    constructor Create(const AOwnTree: IJclIntegerCollection; ACursor: TJclIntegerBinaryNode; AValid: Boolean; AStart: TItrStart);
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
    { IJclIntegerBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Integer;
    function Right: Integer;
  end;

  TJclPreOrderIntegerBinaryTreeIterator = class(TJclIntegerBinaryTreeIterator, IJclIntegerIterator, IJclIntegerTreeIterator, IJclIntegerBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntegerBinaryNode; override;
    function GetPreviousCursor: TJclIntegerBinaryNode; override;
  end;

  TJclInOrderIntegerBinaryTreeIterator = class(TJclIntegerBinaryTreeIterator, IJclIntegerIterator, IJclIntegerTreeIterator, IJclIntegerBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntegerBinaryNode; override;
    function GetPreviousCursor: TJclIntegerBinaryNode; override;
  end;

  TJclPostOrderIntegerBinaryTreeIterator = class(TJclIntegerBinaryTreeIterator, IJclIntegerIterator, IJclIntegerTreeIterator, IJclIntegerBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntegerBinaryNode; override;
    function GetPreviousCursor: TJclIntegerBinaryNode; override;
  end;

  TJclCardinalBinaryNode = class
  public
    Value: Cardinal;
    Left: TJclCardinalBinaryNode;
    Right: TJclCardinalBinaryNode;
    Parent: TJclCardinalBinaryNode;
  end;

  TJclCardinalBinaryTree = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclBaseContainer,
    IJclCardinalEqualityComparer, IJclCardinalComparer, IJclCardinalContainer, IJclCardinalFlatContainer,
    IJclCardinalCollection, IJclCardinalTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FMaxDepth: Integer;
    FRoot: TJclCardinalBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    function BuildTree(const LeafArray: array of TJclCardinalBinaryNode; Left, Right: Integer; Parent: TJclCardinalBinaryNode;
      Offset: Integer): TJclCardinalBinaryNode;
    function CloneNode(Node, Parent: TJclCardinalBinaryNode): TJclCardinalBinaryNode;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
  public
    constructor Create(ACompare: TCardinalCompare);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCardinalCollection }
    function Add(AValue: Cardinal): Boolean;
    function AddAll(const ACollection: IJclCardinalCollection): Boolean;
    procedure Clear;
    function Contains(AValue: Cardinal): Boolean;
    function ContainsAll(const ACollection: IJclCardinalCollection): Boolean;
    function CollectionEquals(const ACollection: IJclCardinalCollection): Boolean;
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

  TJclCardinalBinaryTreeIterator = class(TJclAbstractIterator, IJclCardinalIterator, IJclCardinalTreeIterator, IJclCardinalBinaryTreeIterator)
  protected
    FCursor: TJclCardinalBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclCardinalCollection;
    FEqualityComparer: IJclCardinalEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclCardinalBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclCardinalBinaryNode; virtual; abstract;
  public
    constructor Create(const AOwnTree: IJclCardinalCollection; ACursor: TJclCardinalBinaryNode; AValid: Boolean; AStart: TItrStart);
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
    { IJclCardinalBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Cardinal;
    function Right: Cardinal;
  end;

  TJclPreOrderCardinalBinaryTreeIterator = class(TJclCardinalBinaryTreeIterator, IJclCardinalIterator, IJclCardinalTreeIterator, IJclCardinalBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclCardinalBinaryNode; override;
    function GetPreviousCursor: TJclCardinalBinaryNode; override;
  end;

  TJclInOrderCardinalBinaryTreeIterator = class(TJclCardinalBinaryTreeIterator, IJclCardinalIterator, IJclCardinalTreeIterator, IJclCardinalBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclCardinalBinaryNode; override;
    function GetPreviousCursor: TJclCardinalBinaryNode; override;
  end;

  TJclPostOrderCardinalBinaryTreeIterator = class(TJclCardinalBinaryTreeIterator, IJclCardinalIterator, IJclCardinalTreeIterator, IJclCardinalBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclCardinalBinaryNode; override;
    function GetPreviousCursor: TJclCardinalBinaryNode; override;
  end;

  TJclInt64BinaryNode = class
  public
    Value: Int64;
    Left: TJclInt64BinaryNode;
    Right: TJclInt64BinaryNode;
    Parent: TJclInt64BinaryNode;
  end;

  TJclInt64BinaryTree = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclBaseContainer,
    IJclInt64EqualityComparer, IJclInt64Comparer, IJclInt64Container, IJclInt64FlatContainer,
    IJclInt64Collection, IJclInt64Tree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FMaxDepth: Integer;
    FRoot: TJclInt64BinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    function BuildTree(const LeafArray: array of TJclInt64BinaryNode; Left, Right: Integer; Parent: TJclInt64BinaryNode;
      Offset: Integer): TJclInt64BinaryNode;
    function CloneNode(Node, Parent: TJclInt64BinaryNode): TJclInt64BinaryNode;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
  public
    constructor Create(ACompare: TInt64Compare);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclInt64Collection }
    function Add(const AValue: Int64): Boolean;
    function AddAll(const ACollection: IJclInt64Collection): Boolean;
    procedure Clear;
    function Contains(const AValue: Int64): Boolean;
    function ContainsAll(const ACollection: IJclInt64Collection): Boolean;
    function CollectionEquals(const ACollection: IJclInt64Collection): Boolean;
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

  TJclInt64BinaryTreeIterator = class(TJclAbstractIterator, IJclInt64Iterator, IJclInt64TreeIterator, IJclInt64BinaryTreeIterator)
  protected
    FCursor: TJclInt64BinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclInt64Collection;
    FEqualityComparer: IJclInt64EqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclInt64BinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclInt64BinaryNode; virtual; abstract;
  public
    constructor Create(const AOwnTree: IJclInt64Collection; ACursor: TJclInt64BinaryNode; AValid: Boolean; AStart: TItrStart);
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
    { IJclInt64BinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Int64;
    function Right: Int64;
  end;

  TJclPreOrderInt64BinaryTreeIterator = class(TJclInt64BinaryTreeIterator, IJclInt64Iterator, IJclInt64TreeIterator, IJclInt64BinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclInt64BinaryNode; override;
    function GetPreviousCursor: TJclInt64BinaryNode; override;
  end;

  TJclInOrderInt64BinaryTreeIterator = class(TJclInt64BinaryTreeIterator, IJclInt64Iterator, IJclInt64TreeIterator, IJclInt64BinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclInt64BinaryNode; override;
    function GetPreviousCursor: TJclInt64BinaryNode; override;
  end;

  TJclPostOrderInt64BinaryTreeIterator = class(TJclInt64BinaryTreeIterator, IJclInt64Iterator, IJclInt64TreeIterator, IJclInt64BinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclInt64BinaryNode; override;
    function GetPreviousCursor: TJclInt64BinaryNode; override;
  end;

  TJclPtrBinaryNode = class
  public
    Value: Pointer;
    Left: TJclPtrBinaryNode;
    Right: TJclPtrBinaryNode;
    Parent: TJclPtrBinaryNode;
  end;

  TJclPtrBinaryTree = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclBaseContainer,
    IJclPtrEqualityComparer, IJclPtrComparer, IJclPtrContainer, IJclPtrFlatContainer,
    IJclPtrCollection, IJclPtrTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FMaxDepth: Integer;
    FRoot: TJclPtrBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    function BuildTree(const LeafArray: array of TJclPtrBinaryNode; Left, Right: Integer; Parent: TJclPtrBinaryNode;
      Offset: Integer): TJclPtrBinaryNode;
    function CloneNode(Node, Parent: TJclPtrBinaryNode): TJclPtrBinaryNode;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
  public
    constructor Create(ACompare: TPtrCompare);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclPtrCollection }
    function Add(APtr: Pointer): Boolean;
    function AddAll(const ACollection: IJclPtrCollection): Boolean;
    procedure Clear;
    function Contains(APtr: Pointer): Boolean;
    function ContainsAll(const ACollection: IJclPtrCollection): Boolean;
    function CollectionEquals(const ACollection: IJclPtrCollection): Boolean;
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

  TJclPtrBinaryTreeIterator = class(TJclAbstractIterator, IJclPtrIterator, IJclPtrTreeIterator, IJclPtrBinaryTreeIterator)
  protected
    FCursor: TJclPtrBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclPtrCollection;
    FEqualityComparer: IJclPtrEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclPtrBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclPtrBinaryNode; virtual; abstract;
  public
    constructor Create(const AOwnTree: IJclPtrCollection; ACursor: TJclPtrBinaryNode; AValid: Boolean; AStart: TItrStart);
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
    { IJclPtrBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Pointer;
    function Right: Pointer;
  end;

  TJclPreOrderPtrBinaryTreeIterator = class(TJclPtrBinaryTreeIterator, IJclPtrIterator, IJclPtrTreeIterator, IJclPtrBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclPtrBinaryNode; override;
    function GetPreviousCursor: TJclPtrBinaryNode; override;
  end;

  TJclInOrderPtrBinaryTreeIterator = class(TJclPtrBinaryTreeIterator, IJclPtrIterator, IJclPtrTreeIterator, IJclPtrBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclPtrBinaryNode; override;
    function GetPreviousCursor: TJclPtrBinaryNode; override;
  end;

  TJclPostOrderPtrBinaryTreeIterator = class(TJclPtrBinaryTreeIterator, IJclPtrIterator, IJclPtrTreeIterator, IJclPtrBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclPtrBinaryNode; override;
    function GetPreviousCursor: TJclPtrBinaryNode; override;
  end;

  TJclBinaryNode = class
  public
    Value: TObject;
    Left: TJclBinaryNode;
    Right: TJclBinaryNode;
    Parent: TJclBinaryNode;
  end;

  TJclBinaryTree = class(TJclAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclBaseContainer,
    IJclEqualityComparer, IJclComparer, IJclContainer, IJclFlatContainer, IJclObjectOwner,
    IJclCollection, IJclTree)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FMaxDepth: Integer;
    FRoot: TJclBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    function BuildTree(const LeafArray: array of TJclBinaryNode; Left, Right: Integer; Parent: TJclBinaryNode;
      Offset: Integer): TJclBinaryNode;
    function CloneNode(Node, Parent: TJclBinaryNode): TJclBinaryNode;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
  public
    constructor Create(ACompare: TCompare; AOwnsObjects: Boolean);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCollection }
    function Add(AObject: TObject): Boolean;
    function AddAll(const ACollection: IJclCollection): Boolean;
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function ContainsAll(const ACollection: IJclCollection): Boolean;
    function CollectionEquals(const ACollection: IJclCollection): Boolean;
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

  TJclBinaryTreeIterator = class(TJclAbstractIterator, IJclIterator, IJclTreeIterator, IJclBinaryTreeIterator)
  protected
    FCursor: TJclBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclCollection;
    FEqualityComparer: IJclEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclBinaryNode; virtual; abstract;
  public
    constructor Create(const AOwnTree: IJclCollection; ACursor: TJclBinaryNode; AValid: Boolean; AStart: TItrStart);
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
    { IJclBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: TObject;
    function Right: TObject;
  end;

  TJclPreOrderBinaryTreeIterator = class(TJclBinaryTreeIterator, IJclIterator, IJclTreeIterator, IJclBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode; override;
    function GetPreviousCursor: TJclBinaryNode; override;
  end;

  TJclInOrderBinaryTreeIterator = class(TJclBinaryTreeIterator, IJclIterator, IJclTreeIterator, IJclBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode; override;
    function GetPreviousCursor: TJclBinaryNode; override;
  end;

  TJclPostOrderBinaryTreeIterator = class(TJclBinaryTreeIterator, IJclIterator, IJclTreeIterator, IJclBinaryTreeIterator,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode; override;
    function GetPreviousCursor: TJclBinaryNode; override;
  end;


  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN

  TJclBinaryNode<T> = class
  public
    Value: T;
    Left: TJclBinaryNode<T>;
    Right: TJclBinaryNode<T>;
    Parent: TJclBinaryNode<T>;
  end;

  TJclBinaryTreeIterator<T> = class;
  TJclPreOrderBinaryTreeIterator<T> = class;
  TJclInOrderBinaryTreeIterator<T> = class;
  TJclPostOrderBinaryTreeIterator<T> = class;

  TJclBinaryTree<T> = class(TJclAbstractContainer<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclBaseContainer,
    IJclEqualityComparer<T>, IJclComparer<T>, IJclContainer<T>, IJclFlatContainer<T>,IJclItemOwner<T>,
    IJclCollection<T>, IJclTree<T>)
  protected
    type
      TBinaryNode = TJclBinaryNode<T>;
      TPreOrderBinaryTreeIterator = TJclPreOrderBinaryTreeIterator<T>;
      TInOrderBinaryTreeIterator = TJclInOrderBinaryTreeIterator<T>;
      TPostOrderBinaryTreeIterator = TJclPostOrderBinaryTreeIterator<T>;
  private
    FMaxDepth: Integer;
    FRoot: TBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    function BuildTree(const LeafArray: array of TBinaryNode; Left, Right: Integer; Parent: TBinaryNode;
      Offset: Integer): TBinaryNode;
    function CloneNode(Node, Parent: TBinaryNode): TBinaryNode;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
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
    function Contains(const AItem: T): Boolean;
    function ContainsAll(const ACollection: IJclCollection<T>): Boolean;
    function CollectionEquals(const ACollection: IJclCollection<T>): Boolean;
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

  TJclBinaryTreeIterator<T> = class(TJclAbstractIterator, IJclIterator<T>, IJclTreeIterator<T>, IJclBinaryTreeIterator<T>)
  protected
    FCursor: TJclBinaryNode<T>;
    FStart: TItrStart;
    FOwnTree: IJclCollection<T>;
    FEqualityComparer: IJclEqualityComparer<T>;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclBinaryNode<T>; virtual; abstract;
    function GetPreviousCursor: TJclBinaryNode<T>; virtual; abstract;
  public
    constructor Create(const AOwnTree: IJclCollection<T>; ACursor: TJclBinaryNode<T>; AValid: Boolean; AStart: TItrStart);
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
    { IJclBinaryTreeIterator<T> }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: T;
    function Right: T;
  end;

  TJclPreOrderBinaryTreeIterator<T> = class(TJclBinaryTreeIterator<T>, IJclIterator<T>, IJclTreeIterator<T>, IJclBinaryTreeIterator<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode<T>; override;
    function GetPreviousCursor: TJclBinaryNode<T>; override;
  end;

  TJclInOrderBinaryTreeIterator<T> = class(TJclBinaryTreeIterator<T>, IJclIterator<T>, IJclTreeIterator<T>, IJclBinaryTreeIterator<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode<T>; override;
    function GetPreviousCursor: TJclBinaryNode<T>; override;
  end;

  TJclPostOrderBinaryTreeIterator<T> = class(TJclBinaryTreeIterator<T>, IJclIterator<T>, IJclTreeIterator<T>, IJclBinaryTreeIterator<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode<T>; override;
    function GetPreviousCursor: TJclBinaryNode<T>; override;
  end;

  // E = External helper to compare items
  TJclBinaryTreeE<T> = class(TJclBinaryTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclContainer<T>, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  private
    FComparer: IJclComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AComparer: IJclComparer<T>; AOwnsItems: Boolean);
    { IJclComparer<T> }
    function ItemsCompare(const A, B: T): Integer; override;
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    property Comparer: IJclComparer<T> read FComparer write FComparer;
  end;

  // F = Function to compare items
  TJclBinaryTreeF<T> = class(TJclBinaryTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclContainer<T>, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TCompare<T>; AOwnsItems: Boolean);
  end;

  // I = Items can compare themselves to an other
  TJclBinaryTreeI<T: IComparable<T>> = class(TJclBinaryTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclContainer<T>, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    { IJclComparer<T> }
    function ItemsCompare(const A, B: T): Integer; override;
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
  end;

  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

{$IFDEF BCB}
{$IFDEF WIN64}
  {$HPPEMIT '#ifdef MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT ' #undef MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT ' #define JclBinaryTrees_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT '#endif'}

  {$HPPEMIT END '#ifdef JclBinaryTrees_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END ' #define MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END ' #undef JclBinaryTrees_MANAGED_INTERFACE_OPERATORS'}
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

//=== { TJclIntfBinaryTree } =================================================

constructor TJclIntfBinaryTree.Create(ACompare: TIntfCompare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclIntfBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclIntfBinaryTree.Add(const AInterface: IInterface): Boolean;
var
  NewNode, Current, Save: TJclIntfBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AInterface, nil) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AInterface, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclIntfBinaryNode.Create;
        NewNode.Value := AInterface;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.AddAll(const ACollection: IJclIntfCollection): Boolean;
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

procedure TJclIntfBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclIntfBinaryTree;
  ACollection: IJclIntfCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfBinaryTree then
  begin
    ADest := TJclIntfBinaryTree(Dest);
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

procedure TJclIntfBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfBinaryTree then
    TJclIntfBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclIntfBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

function TJclIntfBinaryTree.BuildTree(const LeafArray: array of TJclIntfBinaryNode; Left, Right: Integer; Parent: TJclIntfBinaryNode;
  Offset: Integer): TJclIntfBinaryNode;
var
  Middle: Integer;
begin
  Middle := (Left + Right + Offset) shr 1;
  Result := LeafArray[Middle];
  Result.Parent := Parent;
  if Middle > Left then
    Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
  else
    Result.Left := nil;
  if Middle < Right then
    Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
  else
    Result.Right := nil;
end;

procedure TJclIntfBinaryTree.Clear;
var
  Current, Parent: TJclIntfBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeObject(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.CloneNode(Node, Parent: TJclIntfBinaryNode): TJclIntfBinaryNode;
begin
  Result := TJclIntfBinaryNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  if Node.Left <> nil then
    Result.Left := CloneNode(Node.Left, Result); // recursive call
  if Node.Right <> nil then
    Result.Right := CloneNode(Node.Right, Result); // recursive call
end;

function TJclIntfBinaryTree.CollectionEquals(const ACollection: IJclIntfCollection): Boolean;
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

function TJclIntfBinaryTree.Contains(const AInterface: IInterface): Boolean;
var
  Comp: Integer;
  Current: TJclIntfBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AInterface);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.ContainsAll(const ACollection: IJclIntfCollection): Boolean;
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

function TJclIntfBinaryTree.Extract(const AInterface: IInterface): Boolean;
var
  Current, Successor: TJclIntfBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AInterface in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AInterface, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      Current.Value := nil;
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.ExtractAll(const ACollection: IJclIntfCollection): Boolean;
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

function TJclIntfBinaryTree.First: IJclIntfIterator;
var
  Start: TJclIntfBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderIntfBinaryTreeIterator.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TJclInOrderIntfBinaryTreeIterator.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TJclPostOrderIntfBinaryTreeIterator.Create(Self, Start, False, isFirst);
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
function TJclIntfBinaryTree.GetEnumerator: IJclIntfIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntfBinaryTree.GetRoot: IJclIntfTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderIntfBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TJclInOrderIntfBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderIntfBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
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

function TJclIntfBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclIntfBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfBinaryTree.Last: IJclIntfIterator;
var
  Start: TJclIntfBinaryNode;
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
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TJclPreOrderIntfBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TJclInOrderIntfBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderIntfBinaryTreeIterator.Create(Self, Start, False, isLast);
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

procedure TJclIntfBinaryTree.Pack;
var
  LeafArray: array of TJclIntfBinaryNode;
  ANode, BNode: TJclIntfBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.Remove(const AInterface: IInterface): Boolean;
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

function TJclIntfBinaryTree.RemoveAll(const ACollection: IJclIntfCollection): Boolean;
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

function TJclIntfBinaryTree.RetainAll(const ACollection: IJclIntfCollection): Boolean;
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

procedure TJclIntfBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntfBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclIntfBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

//=== { TJclIntfBinaryTreeIterator } ===========================================================

constructor TJclIntfBinaryTreeIterator.Create(const AOwnTree: IJclIntfCollection; ACursor: TJclIntfBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclIntfEqualityComparer;
end;

function TJclIntfBinaryTreeIterator.Add(const AInterface: IInterface): Boolean;
begin
  Result := FOwnTree.Add(AInterface);
end;

function TJclIntfBinaryTreeIterator.AddChild(const AInterface: IInterface): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntfBinaryTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclIntfBinaryTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclIntfBinaryTreeIterator then
  begin
    ADest := TJclIntfBinaryTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclIntfBinaryTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfBinaryTreeIterator.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntfBinaryTreeIterator.DeleteChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntfBinaryTreeIterator.Extract;
var
  OldCursor: TJclIntfBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Extract(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfBinaryTreeIterator.ExtractChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntfBinaryTreeIterator.ExtractChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclIntfBinaryTreeIterator.GetChild(Index: Integer): IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
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

function TJclIntfBinaryTreeIterator.GetObject: IInterface;
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

function TJclIntfBinaryTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTreeIterator.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTreeIterator.HasNext: Boolean;
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

function TJclIntfBinaryTreeIterator.HasParent: Boolean;
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

function TJclIntfBinaryTreeIterator.HasPrevious: Boolean;
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

function TJclIntfBinaryTreeIterator.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTreeIterator.IndexOfChild(const AInterface: IInterface): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AInterface) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AInterface) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AInterface) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTreeIterator.Insert(const AInterface: IInterface): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclIntfBinaryTreeIterator.InsertChild(Index: Integer; const AInterface: IInterface): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclIntfBinaryTreeIterator.IteratorEquals(const AIterator: IJclIntfIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclIntfBinaryTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclIntfBinaryTreeIterator then
  begin
    ItrObj := TJclIntfBinaryTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TJclIntfBinaryTreeIterator.Left: IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FCursor <> nil then
      FCursor := FCursor.Left;
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

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntfBinaryTreeIterator.MoveNext: Boolean;
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

function TJclIntfBinaryTreeIterator.Next: IInterface;
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

function TJclIntfBinaryTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclIntfBinaryTreeIterator.Parent: IInterface;
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

function TJclIntfBinaryTreeIterator.Previous: IInterface;
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

function TJclIntfBinaryTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntfBinaryTreeIterator.Remove;
var
  OldCursor: TJclIntfBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfBinaryTreeIterator.Reset;
var
  NewCursor: TJclIntfBinaryNode;
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

function TJclIntfBinaryTreeIterator.Right: IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FCursor <> nil then
      FCursor := FCursor.Right;
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

procedure TJclIntfBinaryTreeIterator.SetChild(Index: Integer; const AInterface: IInterface);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntfBinaryTreeIterator.SetObject(const AInterface: IInterface);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TJclPreOrderIntfBinaryTreeIterator } ===================================================

function TJclPreOrderIntfBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderIntfBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderIntfBinaryTreeIterator.GetNextCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TJclPreOrderIntfBinaryTreeIterator.GetPreviousCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TJclInOrderIntfBinaryTreeIterator } ====================================================

function TJclInOrderIntfBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclInOrderIntfBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclInOrderIntfBinaryTreeIterator.GetNextCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TJclInOrderIntfBinaryTreeIterator.GetPreviousCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TJclPostOrderIntfBinaryTreeIterator } ==================================================

function TJclPostOrderIntfBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderIntfBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderIntfBinaryTreeIterator.GetNextCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TJclPostOrderIntfBinaryTreeIterator.GetPreviousCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TJclAnsiStrBinaryTree } =================================================

constructor TJclAnsiStrBinaryTree.Create(ACompare: TAnsiStrCompare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclAnsiStrBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclAnsiStrBinaryTree.Add(const AString: AnsiString): Boolean;
var
  NewNode, Current, Save: TJclAnsiStrBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AString, '') then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AString, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclAnsiStrBinaryNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.AddAll(const ACollection: IJclAnsiStrCollection): Boolean;
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

procedure TJclAnsiStrBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclAnsiStrBinaryTree;
  ACollection: IJclAnsiStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrBinaryTree then
  begin
    ADest := TJclAnsiStrBinaryTree(Dest);
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

procedure TJclAnsiStrBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclAnsiStrBinaryTree then
    TJclAnsiStrBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclAnsiStrBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

function TJclAnsiStrBinaryTree.BuildTree(const LeafArray: array of TJclAnsiStrBinaryNode; Left, Right: Integer; Parent: TJclAnsiStrBinaryNode;
  Offset: Integer): TJclAnsiStrBinaryNode;
var
  Middle: Integer;
begin
  Middle := (Left + Right + Offset) shr 1;
  Result := LeafArray[Middle];
  Result.Parent := Parent;
  if Middle > Left then
    Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
  else
    Result.Left := nil;
  if Middle < Right then
    Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
  else
    Result.Right := nil;
end;

procedure TJclAnsiStrBinaryTree.Clear;
var
  Current, Parent: TJclAnsiStrBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeString(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.CloneNode(Node, Parent: TJclAnsiStrBinaryNode): TJclAnsiStrBinaryNode;
begin
  Result := TJclAnsiStrBinaryNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  if Node.Left <> nil then
    Result.Left := CloneNode(Node.Left, Result); // recursive call
  if Node.Right <> nil then
    Result.Right := CloneNode(Node.Right, Result); // recursive call
end;

function TJclAnsiStrBinaryTree.CollectionEquals(const ACollection: IJclAnsiStrCollection): Boolean;
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

function TJclAnsiStrBinaryTree.Contains(const AString: AnsiString): Boolean;
var
  Comp: Integer;
  Current: TJclAnsiStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AString);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean;
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

function TJclAnsiStrBinaryTree.Extract(const AString: AnsiString): Boolean;
var
  Current, Successor: TJclAnsiStrBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AString in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AString, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      Current.Value := '';
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.ExtractAll(const ACollection: IJclAnsiStrCollection): Boolean;
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

function TJclAnsiStrBinaryTree.First: IJclAnsiStrIterator;
var
  Start: TJclAnsiStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderAnsiStrBinaryTreeIterator.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TJclInOrderAnsiStrBinaryTreeIterator.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TJclPostOrderAnsiStrBinaryTreeIterator.Create(Self, Start, False, isFirst);
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
function TJclAnsiStrBinaryTree.GetEnumerator: IJclAnsiStrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclAnsiStrBinaryTree.GetRoot: IJclAnsiStrTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderAnsiStrBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TJclInOrderAnsiStrBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderAnsiStrBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
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

function TJclAnsiStrBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclAnsiStrBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrBinaryTree.Last: IJclAnsiStrIterator;
var
  Start: TJclAnsiStrBinaryNode;
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
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TJclPreOrderAnsiStrBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TJclInOrderAnsiStrBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderAnsiStrBinaryTreeIterator.Create(Self, Start, False, isLast);
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

procedure TJclAnsiStrBinaryTree.Pack;
var
  LeafArray: array of TJclAnsiStrBinaryNode;
  ANode, BNode: TJclAnsiStrBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.Remove(const AString: AnsiString): Boolean;
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

function TJclAnsiStrBinaryTree.RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean;
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

function TJclAnsiStrBinaryTree.RetainAll(const ACollection: IJclAnsiStrCollection): Boolean;
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

procedure TJclAnsiStrBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclAnsiStrBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclAnsiStrBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

//=== { TJclAnsiStrBinaryTreeIterator } ===========================================================

constructor TJclAnsiStrBinaryTreeIterator.Create(const AOwnTree: IJclAnsiStrCollection; ACursor: TJclAnsiStrBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclAnsiStrEqualityComparer;
end;

function TJclAnsiStrBinaryTreeIterator.Add(const AString: AnsiString): Boolean;
begin
  Result := FOwnTree.Add(AString);
end;

function TJclAnsiStrBinaryTreeIterator.AddChild(const AString: AnsiString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclAnsiStrBinaryTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclAnsiStrBinaryTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclAnsiStrBinaryTreeIterator then
  begin
    ADest := TJclAnsiStrBinaryTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclAnsiStrBinaryTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrBinaryTreeIterator.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclAnsiStrBinaryTreeIterator.DeleteChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclAnsiStrBinaryTreeIterator.Extract;
var
  OldCursor: TJclAnsiStrBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Extract(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrBinaryTreeIterator.ExtractChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclAnsiStrBinaryTreeIterator.ExtractChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclAnsiStrBinaryTreeIterator.GetChild(Index: Integer): AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
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

function TJclAnsiStrBinaryTreeIterator.GetString: AnsiString;
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

function TJclAnsiStrBinaryTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTreeIterator.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTreeIterator.HasNext: Boolean;
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

function TJclAnsiStrBinaryTreeIterator.HasParent: Boolean;
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

function TJclAnsiStrBinaryTreeIterator.HasPrevious: Boolean;
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

function TJclAnsiStrBinaryTreeIterator.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTreeIterator.IndexOfChild(const AString: AnsiString): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AString) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AString) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AString) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTreeIterator.Insert(const AString: AnsiString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclAnsiStrBinaryTreeIterator.InsertChild(Index: Integer; const AString: AnsiString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclAnsiStrBinaryTreeIterator.IteratorEquals(const AIterator: IJclAnsiStrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclAnsiStrBinaryTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclAnsiStrBinaryTreeIterator then
  begin
    ItrObj := TJclAnsiStrBinaryTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TJclAnsiStrBinaryTreeIterator.Left: AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FCursor <> nil then
      FCursor := FCursor.Left;
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

{$IFDEF SUPPORTS_FOR_IN}
function TJclAnsiStrBinaryTreeIterator.MoveNext: Boolean;
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

function TJclAnsiStrBinaryTreeIterator.Next: AnsiString;
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

function TJclAnsiStrBinaryTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclAnsiStrBinaryTreeIterator.Parent: AnsiString;
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

function TJclAnsiStrBinaryTreeIterator.Previous: AnsiString;
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

function TJclAnsiStrBinaryTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclAnsiStrBinaryTreeIterator.Remove;
var
  OldCursor: TJclAnsiStrBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrBinaryTreeIterator.Reset;
var
  NewCursor: TJclAnsiStrBinaryNode;
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

function TJclAnsiStrBinaryTreeIterator.Right: AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FCursor <> nil then
      FCursor := FCursor.Right;
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

procedure TJclAnsiStrBinaryTreeIterator.SetChild(Index: Integer; const AString: AnsiString);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclAnsiStrBinaryTreeIterator.SetString(const AString: AnsiString);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TJclPreOrderAnsiStrBinaryTreeIterator } ===================================================

function TJclPreOrderAnsiStrBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderAnsiStrBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderAnsiStrBinaryTreeIterator.GetNextCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TJclPreOrderAnsiStrBinaryTreeIterator.GetPreviousCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TJclInOrderAnsiStrBinaryTreeIterator } ====================================================

function TJclInOrderAnsiStrBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclInOrderAnsiStrBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclInOrderAnsiStrBinaryTreeIterator.GetNextCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TJclInOrderAnsiStrBinaryTreeIterator.GetPreviousCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TJclPostOrderAnsiStrBinaryTreeIterator } ==================================================

function TJclPostOrderAnsiStrBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderAnsiStrBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderAnsiStrBinaryTreeIterator.GetNextCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TJclPostOrderAnsiStrBinaryTreeIterator.GetPreviousCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TJclWideStrBinaryTree } =================================================

constructor TJclWideStrBinaryTree.Create(ACompare: TWideStrCompare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclWideStrBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclWideStrBinaryTree.Add(const AString: WideString): Boolean;
var
  NewNode, Current, Save: TJclWideStrBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AString, '') then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AString, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclWideStrBinaryNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.AddAll(const ACollection: IJclWideStrCollection): Boolean;
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

procedure TJclWideStrBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclWideStrBinaryTree;
  ACollection: IJclWideStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrBinaryTree then
  begin
    ADest := TJclWideStrBinaryTree(Dest);
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

procedure TJclWideStrBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclWideStrBinaryTree then
    TJclWideStrBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclWideStrBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

function TJclWideStrBinaryTree.BuildTree(const LeafArray: array of TJclWideStrBinaryNode; Left, Right: Integer; Parent: TJclWideStrBinaryNode;
  Offset: Integer): TJclWideStrBinaryNode;
var
  Middle: Integer;
begin
  Middle := (Left + Right + Offset) shr 1;
  Result := LeafArray[Middle];
  Result.Parent := Parent;
  if Middle > Left then
    Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
  else
    Result.Left := nil;
  if Middle < Right then
    Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
  else
    Result.Right := nil;
end;

procedure TJclWideStrBinaryTree.Clear;
var
  Current, Parent: TJclWideStrBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeString(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.CloneNode(Node, Parent: TJclWideStrBinaryNode): TJclWideStrBinaryNode;
begin
  Result := TJclWideStrBinaryNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  if Node.Left <> nil then
    Result.Left := CloneNode(Node.Left, Result); // recursive call
  if Node.Right <> nil then
    Result.Right := CloneNode(Node.Right, Result); // recursive call
end;

function TJclWideStrBinaryTree.CollectionEquals(const ACollection: IJclWideStrCollection): Boolean;
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

function TJclWideStrBinaryTree.Contains(const AString: WideString): Boolean;
var
  Comp: Integer;
  Current: TJclWideStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AString);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.ContainsAll(const ACollection: IJclWideStrCollection): Boolean;
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

function TJclWideStrBinaryTree.Extract(const AString: WideString): Boolean;
var
  Current, Successor: TJclWideStrBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AString in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AString, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      Current.Value := '';
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.ExtractAll(const ACollection: IJclWideStrCollection): Boolean;
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

function TJclWideStrBinaryTree.First: IJclWideStrIterator;
var
  Start: TJclWideStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderWideStrBinaryTreeIterator.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TJclInOrderWideStrBinaryTreeIterator.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TJclPostOrderWideStrBinaryTreeIterator.Create(Self, Start, False, isFirst);
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
function TJclWideStrBinaryTree.GetEnumerator: IJclWideStrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclWideStrBinaryTree.GetRoot: IJclWideStrTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderWideStrBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TJclInOrderWideStrBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderWideStrBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
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

function TJclWideStrBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclWideStrBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrBinaryTree.Last: IJclWideStrIterator;
var
  Start: TJclWideStrBinaryNode;
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
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TJclPreOrderWideStrBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TJclInOrderWideStrBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderWideStrBinaryTreeIterator.Create(Self, Start, False, isLast);
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

procedure TJclWideStrBinaryTree.Pack;
var
  LeafArray: array of TJclWideStrBinaryNode;
  ANode, BNode: TJclWideStrBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.Remove(const AString: WideString): Boolean;
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

function TJclWideStrBinaryTree.RemoveAll(const ACollection: IJclWideStrCollection): Boolean;
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

function TJclWideStrBinaryTree.RetainAll(const ACollection: IJclWideStrCollection): Boolean;
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

procedure TJclWideStrBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclWideStrBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclWideStrBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

//=== { TJclWideStrBinaryTreeIterator } ===========================================================

constructor TJclWideStrBinaryTreeIterator.Create(const AOwnTree: IJclWideStrCollection; ACursor: TJclWideStrBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclWideStrEqualityComparer;
end;

function TJclWideStrBinaryTreeIterator.Add(const AString: WideString): Boolean;
begin
  Result := FOwnTree.Add(AString);
end;

function TJclWideStrBinaryTreeIterator.AddChild(const AString: WideString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclWideStrBinaryTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclWideStrBinaryTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclWideStrBinaryTreeIterator then
  begin
    ADest := TJclWideStrBinaryTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclWideStrBinaryTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrBinaryTreeIterator.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclWideStrBinaryTreeIterator.DeleteChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclWideStrBinaryTreeIterator.Extract;
var
  OldCursor: TJclWideStrBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Extract(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrBinaryTreeIterator.ExtractChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclWideStrBinaryTreeIterator.ExtractChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclWideStrBinaryTreeIterator.GetChild(Index: Integer): WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
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

function TJclWideStrBinaryTreeIterator.GetString: WideString;
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

function TJclWideStrBinaryTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTreeIterator.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTreeIterator.HasNext: Boolean;
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

function TJclWideStrBinaryTreeIterator.HasParent: Boolean;
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

function TJclWideStrBinaryTreeIterator.HasPrevious: Boolean;
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

function TJclWideStrBinaryTreeIterator.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTreeIterator.IndexOfChild(const AString: WideString): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AString) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AString) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AString) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTreeIterator.Insert(const AString: WideString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclWideStrBinaryTreeIterator.InsertChild(Index: Integer; const AString: WideString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclWideStrBinaryTreeIterator.IteratorEquals(const AIterator: IJclWideStrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclWideStrBinaryTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclWideStrBinaryTreeIterator then
  begin
    ItrObj := TJclWideStrBinaryTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TJclWideStrBinaryTreeIterator.Left: WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FCursor <> nil then
      FCursor := FCursor.Left;
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

{$IFDEF SUPPORTS_FOR_IN}
function TJclWideStrBinaryTreeIterator.MoveNext: Boolean;
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

function TJclWideStrBinaryTreeIterator.Next: WideString;
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

function TJclWideStrBinaryTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclWideStrBinaryTreeIterator.Parent: WideString;
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

function TJclWideStrBinaryTreeIterator.Previous: WideString;
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

function TJclWideStrBinaryTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclWideStrBinaryTreeIterator.Remove;
var
  OldCursor: TJclWideStrBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrBinaryTreeIterator.Reset;
var
  NewCursor: TJclWideStrBinaryNode;
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

function TJclWideStrBinaryTreeIterator.Right: WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FCursor <> nil then
      FCursor := FCursor.Right;
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

procedure TJclWideStrBinaryTreeIterator.SetChild(Index: Integer; const AString: WideString);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclWideStrBinaryTreeIterator.SetString(const AString: WideString);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TJclPreOrderWideStrBinaryTreeIterator } ===================================================

function TJclPreOrderWideStrBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderWideStrBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderWideStrBinaryTreeIterator.GetNextCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TJclPreOrderWideStrBinaryTreeIterator.GetPreviousCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TJclInOrderWideStrBinaryTreeIterator } ====================================================

function TJclInOrderWideStrBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclInOrderWideStrBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclInOrderWideStrBinaryTreeIterator.GetNextCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TJclInOrderWideStrBinaryTreeIterator.GetPreviousCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TJclPostOrderWideStrBinaryTreeIterator } ==================================================

function TJclPostOrderWideStrBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderWideStrBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderWideStrBinaryTreeIterator.GetNextCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TJclPostOrderWideStrBinaryTreeIterator.GetPreviousCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrBinaryTree } =================================================

constructor TJclUnicodeStrBinaryTree.Create(ACompare: TUnicodeStrCompare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclUnicodeStrBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclUnicodeStrBinaryTree.Add(const AString: UnicodeString): Boolean;
var
  NewNode, Current, Save: TJclUnicodeStrBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AString, '') then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AString, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclUnicodeStrBinaryNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrBinaryTree.AddAll(const ACollection: IJclUnicodeStrCollection): Boolean;
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

procedure TJclUnicodeStrBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclUnicodeStrBinaryTree;
  ACollection: IJclUnicodeStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclUnicodeStrBinaryTree then
  begin
    ADest := TJclUnicodeStrBinaryTree(Dest);
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

procedure TJclUnicodeStrBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclUnicodeStrBinaryTree then
    TJclUnicodeStrBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclUnicodeStrBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

function TJclUnicodeStrBinaryTree.BuildTree(const LeafArray: array of TJclUnicodeStrBinaryNode; Left, Right: Integer; Parent: TJclUnicodeStrBinaryNode;
  Offset: Integer): TJclUnicodeStrBinaryNode;
var
  Middle: Integer;
begin
  Middle := (Left + Right + Offset) shr 1;
  Result := LeafArray[Middle];
  Result.Parent := Parent;
  if Middle > Left then
    Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
  else
    Result.Left := nil;
  if Middle < Right then
    Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
  else
    Result.Right := nil;
end;

procedure TJclUnicodeStrBinaryTree.Clear;
var
  Current, Parent: TJclUnicodeStrBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeString(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrBinaryTree.CloneNode(Node, Parent: TJclUnicodeStrBinaryNode): TJclUnicodeStrBinaryNode;
begin
  Result := TJclUnicodeStrBinaryNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  if Node.Left <> nil then
    Result.Left := CloneNode(Node.Left, Result); // recursive call
  if Node.Right <> nil then
    Result.Right := CloneNode(Node.Right, Result); // recursive call
end;

function TJclUnicodeStrBinaryTree.CollectionEquals(const ACollection: IJclUnicodeStrCollection): Boolean;
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

function TJclUnicodeStrBinaryTree.Contains(const AString: UnicodeString): Boolean;
var
  Comp: Integer;
  Current: TJclUnicodeStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AString);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrBinaryTree.ContainsAll(const ACollection: IJclUnicodeStrCollection): Boolean;
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

function TJclUnicodeStrBinaryTree.Extract(const AString: UnicodeString): Boolean;
var
  Current, Successor: TJclUnicodeStrBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AString in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AString, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      Current.Value := '';
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrBinaryTree.ExtractAll(const ACollection: IJclUnicodeStrCollection): Boolean;
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

function TJclUnicodeStrBinaryTree.First: IJclUnicodeStrIterator;
var
  Start: TJclUnicodeStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderUnicodeStrBinaryTreeIterator.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TJclInOrderUnicodeStrBinaryTreeIterator.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TJclPostOrderUnicodeStrBinaryTreeIterator.Create(Self, Start, False, isFirst);
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
function TJclUnicodeStrBinaryTree.GetEnumerator: IJclUnicodeStrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclUnicodeStrBinaryTree.GetRoot: IJclUnicodeStrTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderUnicodeStrBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TJclInOrderUnicodeStrBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderUnicodeStrBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
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

function TJclUnicodeStrBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclUnicodeStrBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclUnicodeStrBinaryTree.Last: IJclUnicodeStrIterator;
var
  Start: TJclUnicodeStrBinaryNode;
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
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TJclPreOrderUnicodeStrBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TJclInOrderUnicodeStrBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderUnicodeStrBinaryTreeIterator.Create(Self, Start, False, isLast);
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

procedure TJclUnicodeStrBinaryTree.Pack;
var
  LeafArray: array of TJclUnicodeStrBinaryNode;
  ANode, BNode: TJclUnicodeStrBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrBinaryTree.Remove(const AString: UnicodeString): Boolean;
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

function TJclUnicodeStrBinaryTree.RemoveAll(const ACollection: IJclUnicodeStrCollection): Boolean;
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

function TJclUnicodeStrBinaryTree.RetainAll(const ACollection: IJclUnicodeStrCollection): Boolean;
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

procedure TJclUnicodeStrBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclUnicodeStrBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclUnicodeStrBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclUnicodeStrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

{$ENDIF SUPPORTS_UNICODE_STRING}

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrBinaryTreeIterator } ===========================================================

constructor TJclUnicodeStrBinaryTreeIterator.Create(const AOwnTree: IJclUnicodeStrCollection; ACursor: TJclUnicodeStrBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclUnicodeStrEqualityComparer;
end;

function TJclUnicodeStrBinaryTreeIterator.Add(const AString: UnicodeString): Boolean;
begin
  Result := FOwnTree.Add(AString);
end;

function TJclUnicodeStrBinaryTreeIterator.AddChild(const AString: UnicodeString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclUnicodeStrBinaryTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclUnicodeStrBinaryTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclUnicodeStrBinaryTreeIterator then
  begin
    ADest := TJclUnicodeStrBinaryTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclUnicodeStrBinaryTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrBinaryTreeIterator.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclUnicodeStrBinaryTreeIterator.DeleteChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclUnicodeStrBinaryTreeIterator.Extract;
var
  OldCursor: TJclUnicodeStrBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Extract(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrBinaryTreeIterator.ExtractChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclUnicodeStrBinaryTreeIterator.ExtractChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclUnicodeStrBinaryTreeIterator.GetChild(Index: Integer): UnicodeString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
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

function TJclUnicodeStrBinaryTreeIterator.GetString: UnicodeString;
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

function TJclUnicodeStrBinaryTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrBinaryTreeIterator.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrBinaryTreeIterator.HasNext: Boolean;
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

function TJclUnicodeStrBinaryTreeIterator.HasParent: Boolean;
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

function TJclUnicodeStrBinaryTreeIterator.HasPrevious: Boolean;
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

function TJclUnicodeStrBinaryTreeIterator.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrBinaryTreeIterator.IndexOfChild(const AString: UnicodeString): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AString) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AString) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AString) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrBinaryTreeIterator.Insert(const AString: UnicodeString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclUnicodeStrBinaryTreeIterator.InsertChild(Index: Integer; const AString: UnicodeString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclUnicodeStrBinaryTreeIterator.IteratorEquals(const AIterator: IJclUnicodeStrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclUnicodeStrBinaryTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclUnicodeStrBinaryTreeIterator then
  begin
    ItrObj := TJclUnicodeStrBinaryTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TJclUnicodeStrBinaryTreeIterator.Left: UnicodeString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FCursor <> nil then
      FCursor := FCursor.Left;
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

{$IFDEF SUPPORTS_FOR_IN}
function TJclUnicodeStrBinaryTreeIterator.MoveNext: Boolean;
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

function TJclUnicodeStrBinaryTreeIterator.Next: UnicodeString;
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

function TJclUnicodeStrBinaryTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclUnicodeStrBinaryTreeIterator.Parent: UnicodeString;
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

function TJclUnicodeStrBinaryTreeIterator.Previous: UnicodeString;
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

function TJclUnicodeStrBinaryTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclUnicodeStrBinaryTreeIterator.Remove;
var
  OldCursor: TJclUnicodeStrBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrBinaryTreeIterator.Reset;
var
  NewCursor: TJclUnicodeStrBinaryNode;
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

function TJclUnicodeStrBinaryTreeIterator.Right: UnicodeString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FCursor <> nil then
      FCursor := FCursor.Right;
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

procedure TJclUnicodeStrBinaryTreeIterator.SetChild(Index: Integer; const AString: UnicodeString);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclUnicodeStrBinaryTreeIterator.SetString(const AString: UnicodeString);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TJclPreOrderUnicodeStrBinaryTreeIterator } ===================================================

function TJclPreOrderUnicodeStrBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderUnicodeStrBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderUnicodeStrBinaryTreeIterator.GetNextCursor: TJclUnicodeStrBinaryNode;
var
  LastRet: TJclUnicodeStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TJclPreOrderUnicodeStrBinaryTreeIterator.GetPreviousCursor: TJclUnicodeStrBinaryNode;
var
  LastRet: TJclUnicodeStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TJclInOrderUnicodeStrBinaryTreeIterator } ====================================================

function TJclInOrderUnicodeStrBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclInOrderUnicodeStrBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclInOrderUnicodeStrBinaryTreeIterator.GetNextCursor: TJclUnicodeStrBinaryNode;
var
  LastRet: TJclUnicodeStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TJclInOrderUnicodeStrBinaryTreeIterator.GetPreviousCursor: TJclUnicodeStrBinaryNode;
var
  LastRet: TJclUnicodeStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TJclPostOrderUnicodeStrBinaryTreeIterator } ==================================================

function TJclPostOrderUnicodeStrBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderUnicodeStrBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderUnicodeStrBinaryTreeIterator.GetNextCursor: TJclUnicodeStrBinaryNode;
var
  LastRet: TJclUnicodeStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TJclPostOrderUnicodeStrBinaryTreeIterator.GetPreviousCursor: TJclUnicodeStrBinaryNode;
var
  LastRet: TJclUnicodeStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

//=== { TJclSingleBinaryTree } =================================================

constructor TJclSingleBinaryTree.Create(ACompare: TSingleCompare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclSingleBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclSingleBinaryTree.Add(const AValue: Single): Boolean;
var
  NewNode, Current, Save: TJclSingleBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AValue, 0.0) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclSingleBinaryNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.AddAll(const ACollection: IJclSingleCollection): Boolean;
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

procedure TJclSingleBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclSingleBinaryTree;
  ACollection: IJclSingleCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclSingleBinaryTree then
  begin
    ADest := TJclSingleBinaryTree(Dest);
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

procedure TJclSingleBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclSingleBinaryTree then
    TJclSingleBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclSingleBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

function TJclSingleBinaryTree.BuildTree(const LeafArray: array of TJclSingleBinaryNode; Left, Right: Integer; Parent: TJclSingleBinaryNode;
  Offset: Integer): TJclSingleBinaryNode;
var
  Middle: Integer;
begin
  Middle := (Left + Right + Offset) shr 1;
  Result := LeafArray[Middle];
  Result.Parent := Parent;
  if Middle > Left then
    Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
  else
    Result.Left := nil;
  if Middle < Right then
    Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
  else
    Result.Right := nil;
end;

procedure TJclSingleBinaryTree.Clear;
var
  Current, Parent: TJclSingleBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeSingle(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.CloneNode(Node, Parent: TJclSingleBinaryNode): TJclSingleBinaryNode;
begin
  Result := TJclSingleBinaryNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  if Node.Left <> nil then
    Result.Left := CloneNode(Node.Left, Result); // recursive call
  if Node.Right <> nil then
    Result.Right := CloneNode(Node.Right, Result); // recursive call
end;

function TJclSingleBinaryTree.CollectionEquals(const ACollection: IJclSingleCollection): Boolean;
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

function TJclSingleBinaryTree.Contains(const AValue: Single): Boolean;
var
  Comp: Integer;
  Current: TJclSingleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AValue);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.ContainsAll(const ACollection: IJclSingleCollection): Boolean;
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

function TJclSingleBinaryTree.Extract(const AValue: Single): Boolean;
var
  Current, Successor: TJclSingleBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AValue in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      Current.Value := 0.0;
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.ExtractAll(const ACollection: IJclSingleCollection): Boolean;
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

function TJclSingleBinaryTree.First: IJclSingleIterator;
var
  Start: TJclSingleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderSingleBinaryTreeIterator.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TJclInOrderSingleBinaryTreeIterator.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TJclPostOrderSingleBinaryTreeIterator.Create(Self, Start, False, isFirst);
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
function TJclSingleBinaryTree.GetEnumerator: IJclSingleIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclSingleBinaryTree.GetRoot: IJclSingleTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderSingleBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TJclInOrderSingleBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderSingleBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
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

function TJclSingleBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclSingleBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleBinaryTree.Last: IJclSingleIterator;
var
  Start: TJclSingleBinaryNode;
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
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TJclPreOrderSingleBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TJclInOrderSingleBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderSingleBinaryTreeIterator.Create(Self, Start, False, isLast);
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

procedure TJclSingleBinaryTree.Pack;
var
  LeafArray: array of TJclSingleBinaryNode;
  ANode, BNode: TJclSingleBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.Remove(const AValue: Single): Boolean;
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

function TJclSingleBinaryTree.RemoveAll(const ACollection: IJclSingleCollection): Boolean;
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

function TJclSingleBinaryTree.RetainAll(const ACollection: IJclSingleCollection): Boolean;
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

procedure TJclSingleBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclSingleBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclSingleBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclSingleBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

//=== { TJclSingleBinaryTreeIterator } ===========================================================

constructor TJclSingleBinaryTreeIterator.Create(const AOwnTree: IJclSingleCollection; ACursor: TJclSingleBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclSingleEqualityComparer;
end;

function TJclSingleBinaryTreeIterator.Add(const AValue: Single): Boolean;
begin
  Result := FOwnTree.Add(AValue);
end;

function TJclSingleBinaryTreeIterator.AddChild(const AValue: Single): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclSingleBinaryTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclSingleBinaryTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclSingleBinaryTreeIterator then
  begin
    ADest := TJclSingleBinaryTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclSingleBinaryTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleBinaryTreeIterator.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclSingleBinaryTreeIterator.DeleteChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclSingleBinaryTreeIterator.Extract;
var
  OldCursor: TJclSingleBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Extract(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleBinaryTreeIterator.ExtractChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclSingleBinaryTreeIterator.ExtractChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclSingleBinaryTreeIterator.GetChild(Index: Integer): Single;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
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

function TJclSingleBinaryTreeIterator.GetValue: Single;
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

function TJclSingleBinaryTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTreeIterator.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTreeIterator.HasNext: Boolean;
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

function TJclSingleBinaryTreeIterator.HasParent: Boolean;
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

function TJclSingleBinaryTreeIterator.HasPrevious: Boolean;
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

function TJclSingleBinaryTreeIterator.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTreeIterator.IndexOfChild(const AValue: Single): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AValue) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTreeIterator.Insert(const AValue: Single): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclSingleBinaryTreeIterator.InsertChild(Index: Integer; const AValue: Single): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclSingleBinaryTreeIterator.IteratorEquals(const AIterator: IJclSingleIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclSingleBinaryTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclSingleBinaryTreeIterator then
  begin
    ItrObj := TJclSingleBinaryTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TJclSingleBinaryTreeIterator.Left: Single;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FCursor <> nil then
      FCursor := FCursor.Left;
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

{$IFDEF SUPPORTS_FOR_IN}
function TJclSingleBinaryTreeIterator.MoveNext: Boolean;
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

function TJclSingleBinaryTreeIterator.Next: Single;
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

function TJclSingleBinaryTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclSingleBinaryTreeIterator.Parent: Single;
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

function TJclSingleBinaryTreeIterator.Previous: Single;
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

function TJclSingleBinaryTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclSingleBinaryTreeIterator.Remove;
var
  OldCursor: TJclSingleBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleBinaryTreeIterator.Reset;
var
  NewCursor: TJclSingleBinaryNode;
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

function TJclSingleBinaryTreeIterator.Right: Single;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FCursor <> nil then
      FCursor := FCursor.Right;
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

procedure TJclSingleBinaryTreeIterator.SetChild(Index: Integer; const AValue: Single);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclSingleBinaryTreeIterator.SetValue(const AValue: Single);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TJclPreOrderSingleBinaryTreeIterator } ===================================================

function TJclPreOrderSingleBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderSingleBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderSingleBinaryTreeIterator.GetNextCursor: TJclSingleBinaryNode;
var
  LastRet: TJclSingleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TJclPreOrderSingleBinaryTreeIterator.GetPreviousCursor: TJclSingleBinaryNode;
var
  LastRet: TJclSingleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TJclInOrderSingleBinaryTreeIterator } ====================================================

function TJclInOrderSingleBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclInOrderSingleBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclInOrderSingleBinaryTreeIterator.GetNextCursor: TJclSingleBinaryNode;
var
  LastRet: TJclSingleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TJclInOrderSingleBinaryTreeIterator.GetPreviousCursor: TJclSingleBinaryNode;
var
  LastRet: TJclSingleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TJclPostOrderSingleBinaryTreeIterator } ==================================================

function TJclPostOrderSingleBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderSingleBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderSingleBinaryTreeIterator.GetNextCursor: TJclSingleBinaryNode;
var
  LastRet: TJclSingleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TJclPostOrderSingleBinaryTreeIterator.GetPreviousCursor: TJclSingleBinaryNode;
var
  LastRet: TJclSingleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TJclDoubleBinaryTree } =================================================

constructor TJclDoubleBinaryTree.Create(ACompare: TDoubleCompare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclDoubleBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclDoubleBinaryTree.Add(const AValue: Double): Boolean;
var
  NewNode, Current, Save: TJclDoubleBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AValue, 0.0) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclDoubleBinaryNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.AddAll(const ACollection: IJclDoubleCollection): Boolean;
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

procedure TJclDoubleBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclDoubleBinaryTree;
  ACollection: IJclDoubleCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclDoubleBinaryTree then
  begin
    ADest := TJclDoubleBinaryTree(Dest);
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

procedure TJclDoubleBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclDoubleBinaryTree then
    TJclDoubleBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclDoubleBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

function TJclDoubleBinaryTree.BuildTree(const LeafArray: array of TJclDoubleBinaryNode; Left, Right: Integer; Parent: TJclDoubleBinaryNode;
  Offset: Integer): TJclDoubleBinaryNode;
var
  Middle: Integer;
begin
  Middle := (Left + Right + Offset) shr 1;
  Result := LeafArray[Middle];
  Result.Parent := Parent;
  if Middle > Left then
    Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
  else
    Result.Left := nil;
  if Middle < Right then
    Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
  else
    Result.Right := nil;
end;

procedure TJclDoubleBinaryTree.Clear;
var
  Current, Parent: TJclDoubleBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeDouble(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.CloneNode(Node, Parent: TJclDoubleBinaryNode): TJclDoubleBinaryNode;
begin
  Result := TJclDoubleBinaryNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  if Node.Left <> nil then
    Result.Left := CloneNode(Node.Left, Result); // recursive call
  if Node.Right <> nil then
    Result.Right := CloneNode(Node.Right, Result); // recursive call
end;

function TJclDoubleBinaryTree.CollectionEquals(const ACollection: IJclDoubleCollection): Boolean;
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

function TJclDoubleBinaryTree.Contains(const AValue: Double): Boolean;
var
  Comp: Integer;
  Current: TJclDoubleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AValue);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.ContainsAll(const ACollection: IJclDoubleCollection): Boolean;
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

function TJclDoubleBinaryTree.Extract(const AValue: Double): Boolean;
var
  Current, Successor: TJclDoubleBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AValue in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      Current.Value := 0.0;
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.ExtractAll(const ACollection: IJclDoubleCollection): Boolean;
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

function TJclDoubleBinaryTree.First: IJclDoubleIterator;
var
  Start: TJclDoubleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderDoubleBinaryTreeIterator.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TJclInOrderDoubleBinaryTreeIterator.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TJclPostOrderDoubleBinaryTreeIterator.Create(Self, Start, False, isFirst);
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
function TJclDoubleBinaryTree.GetEnumerator: IJclDoubleIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclDoubleBinaryTree.GetRoot: IJclDoubleTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderDoubleBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TJclInOrderDoubleBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderDoubleBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
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

function TJclDoubleBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclDoubleBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleBinaryTree.Last: IJclDoubleIterator;
var
  Start: TJclDoubleBinaryNode;
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
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TJclPreOrderDoubleBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TJclInOrderDoubleBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderDoubleBinaryTreeIterator.Create(Self, Start, False, isLast);
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

procedure TJclDoubleBinaryTree.Pack;
var
  LeafArray: array of TJclDoubleBinaryNode;
  ANode, BNode: TJclDoubleBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.Remove(const AValue: Double): Boolean;
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

function TJclDoubleBinaryTree.RemoveAll(const ACollection: IJclDoubleCollection): Boolean;
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

function TJclDoubleBinaryTree.RetainAll(const ACollection: IJclDoubleCollection): Boolean;
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

procedure TJclDoubleBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclDoubleBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclDoubleBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclDoubleBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

//=== { TJclDoubleBinaryTreeIterator } ===========================================================

constructor TJclDoubleBinaryTreeIterator.Create(const AOwnTree: IJclDoubleCollection; ACursor: TJclDoubleBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclDoubleEqualityComparer;
end;

function TJclDoubleBinaryTreeIterator.Add(const AValue: Double): Boolean;
begin
  Result := FOwnTree.Add(AValue);
end;

function TJclDoubleBinaryTreeIterator.AddChild(const AValue: Double): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclDoubleBinaryTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclDoubleBinaryTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclDoubleBinaryTreeIterator then
  begin
    ADest := TJclDoubleBinaryTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclDoubleBinaryTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleBinaryTreeIterator.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclDoubleBinaryTreeIterator.DeleteChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclDoubleBinaryTreeIterator.Extract;
var
  OldCursor: TJclDoubleBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Extract(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleBinaryTreeIterator.ExtractChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclDoubleBinaryTreeIterator.ExtractChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclDoubleBinaryTreeIterator.GetChild(Index: Integer): Double;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
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

function TJclDoubleBinaryTreeIterator.GetValue: Double;
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

function TJclDoubleBinaryTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTreeIterator.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTreeIterator.HasNext: Boolean;
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

function TJclDoubleBinaryTreeIterator.HasParent: Boolean;
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

function TJclDoubleBinaryTreeIterator.HasPrevious: Boolean;
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

function TJclDoubleBinaryTreeIterator.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTreeIterator.IndexOfChild(const AValue: Double): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AValue) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTreeIterator.Insert(const AValue: Double): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclDoubleBinaryTreeIterator.InsertChild(Index: Integer; const AValue: Double): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclDoubleBinaryTreeIterator.IteratorEquals(const AIterator: IJclDoubleIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclDoubleBinaryTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclDoubleBinaryTreeIterator then
  begin
    ItrObj := TJclDoubleBinaryTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TJclDoubleBinaryTreeIterator.Left: Double;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FCursor <> nil then
      FCursor := FCursor.Left;
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

{$IFDEF SUPPORTS_FOR_IN}
function TJclDoubleBinaryTreeIterator.MoveNext: Boolean;
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

function TJclDoubleBinaryTreeIterator.Next: Double;
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

function TJclDoubleBinaryTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclDoubleBinaryTreeIterator.Parent: Double;
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

function TJclDoubleBinaryTreeIterator.Previous: Double;
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

function TJclDoubleBinaryTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclDoubleBinaryTreeIterator.Remove;
var
  OldCursor: TJclDoubleBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleBinaryTreeIterator.Reset;
var
  NewCursor: TJclDoubleBinaryNode;
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

function TJclDoubleBinaryTreeIterator.Right: Double;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FCursor <> nil then
      FCursor := FCursor.Right;
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

procedure TJclDoubleBinaryTreeIterator.SetChild(Index: Integer; const AValue: Double);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclDoubleBinaryTreeIterator.SetValue(const AValue: Double);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TJclPreOrderDoubleBinaryTreeIterator } ===================================================

function TJclPreOrderDoubleBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderDoubleBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderDoubleBinaryTreeIterator.GetNextCursor: TJclDoubleBinaryNode;
var
  LastRet: TJclDoubleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TJclPreOrderDoubleBinaryTreeIterator.GetPreviousCursor: TJclDoubleBinaryNode;
var
  LastRet: TJclDoubleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TJclInOrderDoubleBinaryTreeIterator } ====================================================

function TJclInOrderDoubleBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclInOrderDoubleBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclInOrderDoubleBinaryTreeIterator.GetNextCursor: TJclDoubleBinaryNode;
var
  LastRet: TJclDoubleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TJclInOrderDoubleBinaryTreeIterator.GetPreviousCursor: TJclDoubleBinaryNode;
var
  LastRet: TJclDoubleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TJclPostOrderDoubleBinaryTreeIterator } ==================================================

function TJclPostOrderDoubleBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderDoubleBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderDoubleBinaryTreeIterator.GetNextCursor: TJclDoubleBinaryNode;
var
  LastRet: TJclDoubleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TJclPostOrderDoubleBinaryTreeIterator.GetPreviousCursor: TJclDoubleBinaryNode;
var
  LastRet: TJclDoubleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TJclExtendedBinaryTree } =================================================

constructor TJclExtendedBinaryTree.Create(ACompare: TExtendedCompare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclExtendedBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclExtendedBinaryTree.Add(const AValue: Extended): Boolean;
var
  NewNode, Current, Save: TJclExtendedBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AValue, 0.0) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclExtendedBinaryNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.AddAll(const ACollection: IJclExtendedCollection): Boolean;
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

procedure TJclExtendedBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclExtendedBinaryTree;
  ACollection: IJclExtendedCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclExtendedBinaryTree then
  begin
    ADest := TJclExtendedBinaryTree(Dest);
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

procedure TJclExtendedBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclExtendedBinaryTree then
    TJclExtendedBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclExtendedBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

function TJclExtendedBinaryTree.BuildTree(const LeafArray: array of TJclExtendedBinaryNode; Left, Right: Integer; Parent: TJclExtendedBinaryNode;
  Offset: Integer): TJclExtendedBinaryNode;
var
  Middle: Integer;
begin
  Middle := (Left + Right + Offset) shr 1;
  Result := LeafArray[Middle];
  Result.Parent := Parent;
  if Middle > Left then
    Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
  else
    Result.Left := nil;
  if Middle < Right then
    Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
  else
    Result.Right := nil;
end;

procedure TJclExtendedBinaryTree.Clear;
var
  Current, Parent: TJclExtendedBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeExtended(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.CloneNode(Node, Parent: TJclExtendedBinaryNode): TJclExtendedBinaryNode;
begin
  Result := TJclExtendedBinaryNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  if Node.Left <> nil then
    Result.Left := CloneNode(Node.Left, Result); // recursive call
  if Node.Right <> nil then
    Result.Right := CloneNode(Node.Right, Result); // recursive call
end;

function TJclExtendedBinaryTree.CollectionEquals(const ACollection: IJclExtendedCollection): Boolean;
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

function TJclExtendedBinaryTree.Contains(const AValue: Extended): Boolean;
var
  Comp: Integer;
  Current: TJclExtendedBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AValue);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.ContainsAll(const ACollection: IJclExtendedCollection): Boolean;
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

function TJclExtendedBinaryTree.Extract(const AValue: Extended): Boolean;
var
  Current, Successor: TJclExtendedBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AValue in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      Current.Value := 0.0;
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.ExtractAll(const ACollection: IJclExtendedCollection): Boolean;
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

function TJclExtendedBinaryTree.First: IJclExtendedIterator;
var
  Start: TJclExtendedBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderExtendedBinaryTreeIterator.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TJclInOrderExtendedBinaryTreeIterator.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TJclPostOrderExtendedBinaryTreeIterator.Create(Self, Start, False, isFirst);
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
function TJclExtendedBinaryTree.GetEnumerator: IJclExtendedIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclExtendedBinaryTree.GetRoot: IJclExtendedTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderExtendedBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TJclInOrderExtendedBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderExtendedBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
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

function TJclExtendedBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclExtendedBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedBinaryTree.Last: IJclExtendedIterator;
var
  Start: TJclExtendedBinaryNode;
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
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TJclPreOrderExtendedBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TJclInOrderExtendedBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderExtendedBinaryTreeIterator.Create(Self, Start, False, isLast);
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

procedure TJclExtendedBinaryTree.Pack;
var
  LeafArray: array of TJclExtendedBinaryNode;
  ANode, BNode: TJclExtendedBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.Remove(const AValue: Extended): Boolean;
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

function TJclExtendedBinaryTree.RemoveAll(const ACollection: IJclExtendedCollection): Boolean;
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

function TJclExtendedBinaryTree.RetainAll(const ACollection: IJclExtendedCollection): Boolean;
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

procedure TJclExtendedBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclExtendedBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclExtendedBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclExtendedBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

//=== { TJclExtendedBinaryTreeIterator } ===========================================================

constructor TJclExtendedBinaryTreeIterator.Create(const AOwnTree: IJclExtendedCollection; ACursor: TJclExtendedBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclExtendedEqualityComparer;
end;

function TJclExtendedBinaryTreeIterator.Add(const AValue: Extended): Boolean;
begin
  Result := FOwnTree.Add(AValue);
end;

function TJclExtendedBinaryTreeIterator.AddChild(const AValue: Extended): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclExtendedBinaryTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclExtendedBinaryTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclExtendedBinaryTreeIterator then
  begin
    ADest := TJclExtendedBinaryTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclExtendedBinaryTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedBinaryTreeIterator.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclExtendedBinaryTreeIterator.DeleteChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclExtendedBinaryTreeIterator.Extract;
var
  OldCursor: TJclExtendedBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Extract(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedBinaryTreeIterator.ExtractChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclExtendedBinaryTreeIterator.ExtractChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclExtendedBinaryTreeIterator.GetChild(Index: Integer): Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
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

function TJclExtendedBinaryTreeIterator.GetValue: Extended;
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

function TJclExtendedBinaryTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTreeIterator.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTreeIterator.HasNext: Boolean;
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

function TJclExtendedBinaryTreeIterator.HasParent: Boolean;
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

function TJclExtendedBinaryTreeIterator.HasPrevious: Boolean;
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

function TJclExtendedBinaryTreeIterator.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTreeIterator.IndexOfChild(const AValue: Extended): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AValue) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTreeIterator.Insert(const AValue: Extended): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclExtendedBinaryTreeIterator.InsertChild(Index: Integer; const AValue: Extended): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclExtendedBinaryTreeIterator.IteratorEquals(const AIterator: IJclExtendedIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclExtendedBinaryTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclExtendedBinaryTreeIterator then
  begin
    ItrObj := TJclExtendedBinaryTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TJclExtendedBinaryTreeIterator.Left: Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FCursor <> nil then
      FCursor := FCursor.Left;
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

{$IFDEF SUPPORTS_FOR_IN}
function TJclExtendedBinaryTreeIterator.MoveNext: Boolean;
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

function TJclExtendedBinaryTreeIterator.Next: Extended;
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

function TJclExtendedBinaryTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclExtendedBinaryTreeIterator.Parent: Extended;
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

function TJclExtendedBinaryTreeIterator.Previous: Extended;
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

function TJclExtendedBinaryTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclExtendedBinaryTreeIterator.Remove;
var
  OldCursor: TJclExtendedBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedBinaryTreeIterator.Reset;
var
  NewCursor: TJclExtendedBinaryNode;
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

function TJclExtendedBinaryTreeIterator.Right: Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FCursor <> nil then
      FCursor := FCursor.Right;
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

procedure TJclExtendedBinaryTreeIterator.SetChild(Index: Integer; const AValue: Extended);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclExtendedBinaryTreeIterator.SetValue(const AValue: Extended);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TJclPreOrderExtendedBinaryTreeIterator } ===================================================

function TJclPreOrderExtendedBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderExtendedBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderExtendedBinaryTreeIterator.GetNextCursor: TJclExtendedBinaryNode;
var
  LastRet: TJclExtendedBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TJclPreOrderExtendedBinaryTreeIterator.GetPreviousCursor: TJclExtendedBinaryNode;
var
  LastRet: TJclExtendedBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TJclInOrderExtendedBinaryTreeIterator } ====================================================

function TJclInOrderExtendedBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclInOrderExtendedBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclInOrderExtendedBinaryTreeIterator.GetNextCursor: TJclExtendedBinaryNode;
var
  LastRet: TJclExtendedBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TJclInOrderExtendedBinaryTreeIterator.GetPreviousCursor: TJclExtendedBinaryNode;
var
  LastRet: TJclExtendedBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TJclPostOrderExtendedBinaryTreeIterator } ==================================================

function TJclPostOrderExtendedBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderExtendedBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderExtendedBinaryTreeIterator.GetNextCursor: TJclExtendedBinaryNode;
var
  LastRet: TJclExtendedBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TJclPostOrderExtendedBinaryTreeIterator.GetPreviousCursor: TJclExtendedBinaryNode;
var
  LastRet: TJclExtendedBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TJclIntegerBinaryTree } =================================================

constructor TJclIntegerBinaryTree.Create(ACompare: TIntegerCompare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclIntegerBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclIntegerBinaryTree.Add(AValue: Integer): Boolean;
var
  NewNode, Current, Save: TJclIntegerBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AValue, 0) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclIntegerBinaryNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.AddAll(const ACollection: IJclIntegerCollection): Boolean;
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

procedure TJclIntegerBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclIntegerBinaryTree;
  ACollection: IJclIntegerCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntegerBinaryTree then
  begin
    ADest := TJclIntegerBinaryTree(Dest);
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

procedure TJclIntegerBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntegerBinaryTree then
    TJclIntegerBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclIntegerBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

function TJclIntegerBinaryTree.BuildTree(const LeafArray: array of TJclIntegerBinaryNode; Left, Right: Integer; Parent: TJclIntegerBinaryNode;
  Offset: Integer): TJclIntegerBinaryNode;
var
  Middle: Integer;
begin
  Middle := (Left + Right + Offset) shr 1;
  Result := LeafArray[Middle];
  Result.Parent := Parent;
  if Middle > Left then
    Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
  else
    Result.Left := nil;
  if Middle < Right then
    Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
  else
    Result.Right := nil;
end;

procedure TJclIntegerBinaryTree.Clear;
var
  Current, Parent: TJclIntegerBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeInteger(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.CloneNode(Node, Parent: TJclIntegerBinaryNode): TJclIntegerBinaryNode;
begin
  Result := TJclIntegerBinaryNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  if Node.Left <> nil then
    Result.Left := CloneNode(Node.Left, Result); // recursive call
  if Node.Right <> nil then
    Result.Right := CloneNode(Node.Right, Result); // recursive call
end;

function TJclIntegerBinaryTree.CollectionEquals(const ACollection: IJclIntegerCollection): Boolean;
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

function TJclIntegerBinaryTree.Contains(AValue: Integer): Boolean;
var
  Comp: Integer;
  Current: TJclIntegerBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AValue);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.ContainsAll(const ACollection: IJclIntegerCollection): Boolean;
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

function TJclIntegerBinaryTree.Extract(AValue: Integer): Boolean;
var
  Current, Successor: TJclIntegerBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AValue in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      Current.Value := 0;
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.ExtractAll(const ACollection: IJclIntegerCollection): Boolean;
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

function TJclIntegerBinaryTree.First: IJclIntegerIterator;
var
  Start: TJclIntegerBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderIntegerBinaryTreeIterator.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TJclInOrderIntegerBinaryTreeIterator.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TJclPostOrderIntegerBinaryTreeIterator.Create(Self, Start, False, isFirst);
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
function TJclIntegerBinaryTree.GetEnumerator: IJclIntegerIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntegerBinaryTree.GetRoot: IJclIntegerTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderIntegerBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TJclInOrderIntegerBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderIntegerBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
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

function TJclIntegerBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclIntegerBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerBinaryTree.Last: IJclIntegerIterator;
var
  Start: TJclIntegerBinaryNode;
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
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TJclPreOrderIntegerBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TJclInOrderIntegerBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderIntegerBinaryTreeIterator.Create(Self, Start, False, isLast);
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

procedure TJclIntegerBinaryTree.Pack;
var
  LeafArray: array of TJclIntegerBinaryNode;
  ANode, BNode: TJclIntegerBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.Remove(AValue: Integer): Boolean;
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

function TJclIntegerBinaryTree.RemoveAll(const ACollection: IJclIntegerCollection): Boolean;
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

function TJclIntegerBinaryTree.RetainAll(const ACollection: IJclIntegerCollection): Boolean;
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

procedure TJclIntegerBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntegerBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclIntegerBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntegerBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

//=== { TJclIntegerBinaryTreeIterator } ===========================================================

constructor TJclIntegerBinaryTreeIterator.Create(const AOwnTree: IJclIntegerCollection; ACursor: TJclIntegerBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclIntegerEqualityComparer;
end;

function TJclIntegerBinaryTreeIterator.Add(AValue: Integer): Boolean;
begin
  Result := FOwnTree.Add(AValue);
end;

function TJclIntegerBinaryTreeIterator.AddChild(AValue: Integer): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntegerBinaryTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclIntegerBinaryTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclIntegerBinaryTreeIterator then
  begin
    ADest := TJclIntegerBinaryTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclIntegerBinaryTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerBinaryTreeIterator.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntegerBinaryTreeIterator.DeleteChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntegerBinaryTreeIterator.Extract;
var
  OldCursor: TJclIntegerBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Extract(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerBinaryTreeIterator.ExtractChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntegerBinaryTreeIterator.ExtractChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclIntegerBinaryTreeIterator.GetChild(Index: Integer): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
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

function TJclIntegerBinaryTreeIterator.GetValue: Integer;
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

function TJclIntegerBinaryTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTreeIterator.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTreeIterator.HasNext: Boolean;
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

function TJclIntegerBinaryTreeIterator.HasParent: Boolean;
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

function TJclIntegerBinaryTreeIterator.HasPrevious: Boolean;
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

function TJclIntegerBinaryTreeIterator.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTreeIterator.IndexOfChild(AValue: Integer): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AValue) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTreeIterator.Insert(AValue: Integer): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclIntegerBinaryTreeIterator.InsertChild(Index: Integer; AValue: Integer): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclIntegerBinaryTreeIterator.IteratorEquals(const AIterator: IJclIntegerIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclIntegerBinaryTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclIntegerBinaryTreeIterator then
  begin
    ItrObj := TJclIntegerBinaryTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TJclIntegerBinaryTreeIterator.Left: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
      FCursor := FCursor.Left;
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

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntegerBinaryTreeIterator.MoveNext: Boolean;
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

function TJclIntegerBinaryTreeIterator.Next: Integer;
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

function TJclIntegerBinaryTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclIntegerBinaryTreeIterator.Parent: Integer;
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

function TJclIntegerBinaryTreeIterator.Previous: Integer;
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

function TJclIntegerBinaryTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntegerBinaryTreeIterator.Remove;
var
  OldCursor: TJclIntegerBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerBinaryTreeIterator.Reset;
var
  NewCursor: TJclIntegerBinaryNode;
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

function TJclIntegerBinaryTreeIterator.Right: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
      FCursor := FCursor.Right;
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

procedure TJclIntegerBinaryTreeIterator.SetChild(Index: Integer; AValue: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntegerBinaryTreeIterator.SetValue(AValue: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TJclPreOrderIntegerBinaryTreeIterator } ===================================================

function TJclPreOrderIntegerBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderIntegerBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderIntegerBinaryTreeIterator.GetNextCursor: TJclIntegerBinaryNode;
var
  LastRet: TJclIntegerBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TJclPreOrderIntegerBinaryTreeIterator.GetPreviousCursor: TJclIntegerBinaryNode;
var
  LastRet: TJclIntegerBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TJclInOrderIntegerBinaryTreeIterator } ====================================================

function TJclInOrderIntegerBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclInOrderIntegerBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclInOrderIntegerBinaryTreeIterator.GetNextCursor: TJclIntegerBinaryNode;
var
  LastRet: TJclIntegerBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TJclInOrderIntegerBinaryTreeIterator.GetPreviousCursor: TJclIntegerBinaryNode;
var
  LastRet: TJclIntegerBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TJclPostOrderIntegerBinaryTreeIterator } ==================================================

function TJclPostOrderIntegerBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderIntegerBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderIntegerBinaryTreeIterator.GetNextCursor: TJclIntegerBinaryNode;
var
  LastRet: TJclIntegerBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TJclPostOrderIntegerBinaryTreeIterator.GetPreviousCursor: TJclIntegerBinaryNode;
var
  LastRet: TJclIntegerBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TJclCardinalBinaryTree } =================================================

constructor TJclCardinalBinaryTree.Create(ACompare: TCardinalCompare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclCardinalBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclCardinalBinaryTree.Add(AValue: Cardinal): Boolean;
var
  NewNode, Current, Save: TJclCardinalBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AValue, 0) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclCardinalBinaryNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.AddAll(const ACollection: IJclCardinalCollection): Boolean;
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

procedure TJclCardinalBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclCardinalBinaryTree;
  ACollection: IJclCardinalCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclCardinalBinaryTree then
  begin
    ADest := TJclCardinalBinaryTree(Dest);
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

procedure TJclCardinalBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclCardinalBinaryTree then
    TJclCardinalBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclCardinalBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

function TJclCardinalBinaryTree.BuildTree(const LeafArray: array of TJclCardinalBinaryNode; Left, Right: Integer; Parent: TJclCardinalBinaryNode;
  Offset: Integer): TJclCardinalBinaryNode;
var
  Middle: Integer;
begin
  Middle := (Left + Right + Offset) shr 1;
  Result := LeafArray[Middle];
  Result.Parent := Parent;
  if Middle > Left then
    Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
  else
    Result.Left := nil;
  if Middle < Right then
    Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
  else
    Result.Right := nil;
end;

procedure TJclCardinalBinaryTree.Clear;
var
  Current, Parent: TJclCardinalBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeCardinal(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.CloneNode(Node, Parent: TJclCardinalBinaryNode): TJclCardinalBinaryNode;
begin
  Result := TJclCardinalBinaryNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  if Node.Left <> nil then
    Result.Left := CloneNode(Node.Left, Result); // recursive call
  if Node.Right <> nil then
    Result.Right := CloneNode(Node.Right, Result); // recursive call
end;

function TJclCardinalBinaryTree.CollectionEquals(const ACollection: IJclCardinalCollection): Boolean;
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

function TJclCardinalBinaryTree.Contains(AValue: Cardinal): Boolean;
var
  Comp: Integer;
  Current: TJclCardinalBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AValue);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.ContainsAll(const ACollection: IJclCardinalCollection): Boolean;
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

function TJclCardinalBinaryTree.Extract(AValue: Cardinal): Boolean;
var
  Current, Successor: TJclCardinalBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AValue in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      Current.Value := 0;
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.ExtractAll(const ACollection: IJclCardinalCollection): Boolean;
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

function TJclCardinalBinaryTree.First: IJclCardinalIterator;
var
  Start: TJclCardinalBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderCardinalBinaryTreeIterator.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TJclInOrderCardinalBinaryTreeIterator.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TJclPostOrderCardinalBinaryTreeIterator.Create(Self, Start, False, isFirst);
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
function TJclCardinalBinaryTree.GetEnumerator: IJclCardinalIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclCardinalBinaryTree.GetRoot: IJclCardinalTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderCardinalBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TJclInOrderCardinalBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderCardinalBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
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

function TJclCardinalBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclCardinalBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalBinaryTree.Last: IJclCardinalIterator;
var
  Start: TJclCardinalBinaryNode;
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
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TJclPreOrderCardinalBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TJclInOrderCardinalBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderCardinalBinaryTreeIterator.Create(Self, Start, False, isLast);
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

procedure TJclCardinalBinaryTree.Pack;
var
  LeafArray: array of TJclCardinalBinaryNode;
  ANode, BNode: TJclCardinalBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.Remove(AValue: Cardinal): Boolean;
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

function TJclCardinalBinaryTree.RemoveAll(const ACollection: IJclCardinalCollection): Boolean;
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

function TJclCardinalBinaryTree.RetainAll(const ACollection: IJclCardinalCollection): Boolean;
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

procedure TJclCardinalBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclCardinalBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclCardinalBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclCardinalBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

//=== { TJclCardinalBinaryTreeIterator } ===========================================================

constructor TJclCardinalBinaryTreeIterator.Create(const AOwnTree: IJclCardinalCollection; ACursor: TJclCardinalBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclCardinalEqualityComparer;
end;

function TJclCardinalBinaryTreeIterator.Add(AValue: Cardinal): Boolean;
begin
  Result := FOwnTree.Add(AValue);
end;

function TJclCardinalBinaryTreeIterator.AddChild(AValue: Cardinal): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclCardinalBinaryTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclCardinalBinaryTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclCardinalBinaryTreeIterator then
  begin
    ADest := TJclCardinalBinaryTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclCardinalBinaryTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalBinaryTreeIterator.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclCardinalBinaryTreeIterator.DeleteChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclCardinalBinaryTreeIterator.Extract;
var
  OldCursor: TJclCardinalBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Extract(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalBinaryTreeIterator.ExtractChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclCardinalBinaryTreeIterator.ExtractChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclCardinalBinaryTreeIterator.GetChild(Index: Integer): Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
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

function TJclCardinalBinaryTreeIterator.GetValue: Cardinal;
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

function TJclCardinalBinaryTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTreeIterator.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTreeIterator.HasNext: Boolean;
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

function TJclCardinalBinaryTreeIterator.HasParent: Boolean;
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

function TJclCardinalBinaryTreeIterator.HasPrevious: Boolean;
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

function TJclCardinalBinaryTreeIterator.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTreeIterator.IndexOfChild(AValue: Cardinal): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AValue) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTreeIterator.Insert(AValue: Cardinal): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclCardinalBinaryTreeIterator.InsertChild(Index: Integer; AValue: Cardinal): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclCardinalBinaryTreeIterator.IteratorEquals(const AIterator: IJclCardinalIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclCardinalBinaryTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclCardinalBinaryTreeIterator then
  begin
    ItrObj := TJclCardinalBinaryTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TJclCardinalBinaryTreeIterator.Left: Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
      FCursor := FCursor.Left;
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

{$IFDEF SUPPORTS_FOR_IN}
function TJclCardinalBinaryTreeIterator.MoveNext: Boolean;
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

function TJclCardinalBinaryTreeIterator.Next: Cardinal;
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

function TJclCardinalBinaryTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclCardinalBinaryTreeIterator.Parent: Cardinal;
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

function TJclCardinalBinaryTreeIterator.Previous: Cardinal;
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

function TJclCardinalBinaryTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclCardinalBinaryTreeIterator.Remove;
var
  OldCursor: TJclCardinalBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalBinaryTreeIterator.Reset;
var
  NewCursor: TJclCardinalBinaryNode;
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

function TJclCardinalBinaryTreeIterator.Right: Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
      FCursor := FCursor.Right;
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

procedure TJclCardinalBinaryTreeIterator.SetChild(Index: Integer; AValue: Cardinal);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclCardinalBinaryTreeIterator.SetValue(AValue: Cardinal);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TJclPreOrderCardinalBinaryTreeIterator } ===================================================

function TJclPreOrderCardinalBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderCardinalBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderCardinalBinaryTreeIterator.GetNextCursor: TJclCardinalBinaryNode;
var
  LastRet: TJclCardinalBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TJclPreOrderCardinalBinaryTreeIterator.GetPreviousCursor: TJclCardinalBinaryNode;
var
  LastRet: TJclCardinalBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TJclInOrderCardinalBinaryTreeIterator } ====================================================

function TJclInOrderCardinalBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclInOrderCardinalBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclInOrderCardinalBinaryTreeIterator.GetNextCursor: TJclCardinalBinaryNode;
var
  LastRet: TJclCardinalBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TJclInOrderCardinalBinaryTreeIterator.GetPreviousCursor: TJclCardinalBinaryNode;
var
  LastRet: TJclCardinalBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TJclPostOrderCardinalBinaryTreeIterator } ==================================================

function TJclPostOrderCardinalBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderCardinalBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderCardinalBinaryTreeIterator.GetNextCursor: TJclCardinalBinaryNode;
var
  LastRet: TJclCardinalBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TJclPostOrderCardinalBinaryTreeIterator.GetPreviousCursor: TJclCardinalBinaryNode;
var
  LastRet: TJclCardinalBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TJclInt64BinaryTree } =================================================

constructor TJclInt64BinaryTree.Create(ACompare: TInt64Compare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclInt64BinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclInt64BinaryTree.Add(const AValue: Int64): Boolean;
var
  NewNode, Current, Save: TJclInt64BinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AValue, 0) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclInt64BinaryNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.AddAll(const ACollection: IJclInt64Collection): Boolean;
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

procedure TJclInt64BinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclInt64BinaryTree;
  ACollection: IJclInt64Collection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclInt64BinaryTree then
  begin
    ADest := TJclInt64BinaryTree(Dest);
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

procedure TJclInt64BinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclInt64BinaryTree then
    TJclInt64BinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclInt64BinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

function TJclInt64BinaryTree.BuildTree(const LeafArray: array of TJclInt64BinaryNode; Left, Right: Integer; Parent: TJclInt64BinaryNode;
  Offset: Integer): TJclInt64BinaryNode;
var
  Middle: Integer;
begin
  Middle := (Left + Right + Offset) shr 1;
  Result := LeafArray[Middle];
  Result.Parent := Parent;
  if Middle > Left then
    Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
  else
    Result.Left := nil;
  if Middle < Right then
    Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
  else
    Result.Right := nil;
end;

procedure TJclInt64BinaryTree.Clear;
var
  Current, Parent: TJclInt64BinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeInt64(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.CloneNode(Node, Parent: TJclInt64BinaryNode): TJclInt64BinaryNode;
begin
  Result := TJclInt64BinaryNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  if Node.Left <> nil then
    Result.Left := CloneNode(Node.Left, Result); // recursive call
  if Node.Right <> nil then
    Result.Right := CloneNode(Node.Right, Result); // recursive call
end;

function TJclInt64BinaryTree.CollectionEquals(const ACollection: IJclInt64Collection): Boolean;
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

function TJclInt64BinaryTree.Contains(const AValue: Int64): Boolean;
var
  Comp: Integer;
  Current: TJclInt64BinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AValue);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.ContainsAll(const ACollection: IJclInt64Collection): Boolean;
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

function TJclInt64BinaryTree.Extract(const AValue: Int64): Boolean;
var
  Current, Successor: TJclInt64BinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AValue in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      Current.Value := 0;
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.ExtractAll(const ACollection: IJclInt64Collection): Boolean;
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

function TJclInt64BinaryTree.First: IJclInt64Iterator;
var
  Start: TJclInt64BinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderInt64BinaryTreeIterator.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TJclInOrderInt64BinaryTreeIterator.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TJclPostOrderInt64BinaryTreeIterator.Create(Self, Start, False, isFirst);
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
function TJclInt64BinaryTree.GetEnumerator: IJclInt64Iterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclInt64BinaryTree.GetRoot: IJclInt64TreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderInt64BinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TJclInOrderInt64BinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderInt64BinaryTreeIterator.Create(Self, FRoot, False, isRoot);
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

function TJclInt64BinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclInt64BinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64BinaryTree.Last: IJclInt64Iterator;
var
  Start: TJclInt64BinaryNode;
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
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TJclPreOrderInt64BinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TJclInOrderInt64BinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderInt64BinaryTreeIterator.Create(Self, Start, False, isLast);
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

procedure TJclInt64BinaryTree.Pack;
var
  LeafArray: array of TJclInt64BinaryNode;
  ANode, BNode: TJclInt64BinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.Remove(const AValue: Int64): Boolean;
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

function TJclInt64BinaryTree.RemoveAll(const ACollection: IJclInt64Collection): Boolean;
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

function TJclInt64BinaryTree.RetainAll(const ACollection: IJclInt64Collection): Boolean;
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

procedure TJclInt64BinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclInt64BinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclInt64BinaryTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclInt64BinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64BinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

//=== { TJclInt64BinaryTreeIterator } ===========================================================

constructor TJclInt64BinaryTreeIterator.Create(const AOwnTree: IJclInt64Collection; ACursor: TJclInt64BinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclInt64EqualityComparer;
end;

function TJclInt64BinaryTreeIterator.Add(const AValue: Int64): Boolean;
begin
  Result := FOwnTree.Add(AValue);
end;

function TJclInt64BinaryTreeIterator.AddChild(const AValue: Int64): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclInt64BinaryTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclInt64BinaryTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclInt64BinaryTreeIterator then
  begin
    ADest := TJclInt64BinaryTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclInt64BinaryTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64BinaryTreeIterator.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclInt64BinaryTreeIterator.DeleteChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclInt64BinaryTreeIterator.Extract;
var
  OldCursor: TJclInt64BinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Extract(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64BinaryTreeIterator.ExtractChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclInt64BinaryTreeIterator.ExtractChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclInt64BinaryTreeIterator.GetChild(Index: Integer): Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
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

function TJclInt64BinaryTreeIterator.GetValue: Int64;
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

function TJclInt64BinaryTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTreeIterator.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTreeIterator.HasNext: Boolean;
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

function TJclInt64BinaryTreeIterator.HasParent: Boolean;
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

function TJclInt64BinaryTreeIterator.HasPrevious: Boolean;
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

function TJclInt64BinaryTreeIterator.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTreeIterator.IndexOfChild(const AValue: Int64): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AValue) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTreeIterator.Insert(const AValue: Int64): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclInt64BinaryTreeIterator.InsertChild(Index: Integer; const AValue: Int64): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclInt64BinaryTreeIterator.IteratorEquals(const AIterator: IJclInt64Iterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclInt64BinaryTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclInt64BinaryTreeIterator then
  begin
    ItrObj := TJclInt64BinaryTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TJclInt64BinaryTreeIterator.Left: Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
      FCursor := FCursor.Left;
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

{$IFDEF SUPPORTS_FOR_IN}
function TJclInt64BinaryTreeIterator.MoveNext: Boolean;
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

function TJclInt64BinaryTreeIterator.Next: Int64;
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

function TJclInt64BinaryTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclInt64BinaryTreeIterator.Parent: Int64;
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

function TJclInt64BinaryTreeIterator.Previous: Int64;
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

function TJclInt64BinaryTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclInt64BinaryTreeIterator.Remove;
var
  OldCursor: TJclInt64BinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64BinaryTreeIterator.Reset;
var
  NewCursor: TJclInt64BinaryNode;
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

function TJclInt64BinaryTreeIterator.Right: Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
      FCursor := FCursor.Right;
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

procedure TJclInt64BinaryTreeIterator.SetChild(Index: Integer; const AValue: Int64);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclInt64BinaryTreeIterator.SetValue(const AValue: Int64);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TJclPreOrderInt64BinaryTreeIterator } ===================================================

function TJclPreOrderInt64BinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderInt64BinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderInt64BinaryTreeIterator.GetNextCursor: TJclInt64BinaryNode;
var
  LastRet: TJclInt64BinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TJclPreOrderInt64BinaryTreeIterator.GetPreviousCursor: TJclInt64BinaryNode;
var
  LastRet: TJclInt64BinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TJclInOrderInt64BinaryTreeIterator } ====================================================

function TJclInOrderInt64BinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclInOrderInt64BinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclInOrderInt64BinaryTreeIterator.GetNextCursor: TJclInt64BinaryNode;
var
  LastRet: TJclInt64BinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TJclInOrderInt64BinaryTreeIterator.GetPreviousCursor: TJclInt64BinaryNode;
var
  LastRet: TJclInt64BinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TJclPostOrderInt64BinaryTreeIterator } ==================================================

function TJclPostOrderInt64BinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderInt64BinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderInt64BinaryTreeIterator.GetNextCursor: TJclInt64BinaryNode;
var
  LastRet: TJclInt64BinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TJclPostOrderInt64BinaryTreeIterator.GetPreviousCursor: TJclInt64BinaryNode;
var
  LastRet: TJclInt64BinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TJclPtrBinaryTree } =================================================

constructor TJclPtrBinaryTree.Create(ACompare: TPtrCompare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclPtrBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclPtrBinaryTree.Add(APtr: Pointer): Boolean;
var
  NewNode, Current, Save: TJclPtrBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(APtr, nil) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(APtr, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclPtrBinaryNode.Create;
        NewNode.Value := APtr;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.AddAll(const ACollection: IJclPtrCollection): Boolean;
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

procedure TJclPtrBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclPtrBinaryTree;
  ACollection: IJclPtrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclPtrBinaryTree then
  begin
    ADest := TJclPtrBinaryTree(Dest);
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

procedure TJclPtrBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclPtrBinaryTree then
    TJclPtrBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclPtrBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

function TJclPtrBinaryTree.BuildTree(const LeafArray: array of TJclPtrBinaryNode; Left, Right: Integer; Parent: TJclPtrBinaryNode;
  Offset: Integer): TJclPtrBinaryNode;
var
  Middle: Integer;
begin
  Middle := (Left + Right + Offset) shr 1;
  Result := LeafArray[Middle];
  Result.Parent := Parent;
  if Middle > Left then
    Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
  else
    Result.Left := nil;
  if Middle < Right then
    Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
  else
    Result.Right := nil;
end;

procedure TJclPtrBinaryTree.Clear;
var
  Current, Parent: TJclPtrBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreePointer(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.CloneNode(Node, Parent: TJclPtrBinaryNode): TJclPtrBinaryNode;
begin
  Result := TJclPtrBinaryNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  if Node.Left <> nil then
    Result.Left := CloneNode(Node.Left, Result); // recursive call
  if Node.Right <> nil then
    Result.Right := CloneNode(Node.Right, Result); // recursive call
end;

function TJclPtrBinaryTree.CollectionEquals(const ACollection: IJclPtrCollection): Boolean;
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

function TJclPtrBinaryTree.Contains(APtr: Pointer): Boolean;
var
  Comp: Integer;
  Current: TJclPtrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, APtr);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.ContainsAll(const ACollection: IJclPtrCollection): Boolean;
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

function TJclPtrBinaryTree.Extract(APtr: Pointer): Boolean;
var
  Current, Successor: TJclPtrBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate APtr in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(APtr, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      Current.Value := nil;
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.ExtractAll(const ACollection: IJclPtrCollection): Boolean;
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

function TJclPtrBinaryTree.First: IJclPtrIterator;
var
  Start: TJclPtrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderPtrBinaryTreeIterator.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TJclInOrderPtrBinaryTreeIterator.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TJclPostOrderPtrBinaryTreeIterator.Create(Self, Start, False, isFirst);
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
function TJclPtrBinaryTree.GetEnumerator: IJclPtrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclPtrBinaryTree.GetRoot: IJclPtrTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderPtrBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TJclInOrderPtrBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderPtrBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
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

function TJclPtrBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclPtrBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrBinaryTree.Last: IJclPtrIterator;
var
  Start: TJclPtrBinaryNode;
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
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TJclPreOrderPtrBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TJclInOrderPtrBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderPtrBinaryTreeIterator.Create(Self, Start, False, isLast);
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

procedure TJclPtrBinaryTree.Pack;
var
  LeafArray: array of TJclPtrBinaryNode;
  ANode, BNode: TJclPtrBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.Remove(APtr: Pointer): Boolean;
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

function TJclPtrBinaryTree.RemoveAll(const ACollection: IJclPtrCollection): Boolean;
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

function TJclPtrBinaryTree.RetainAll(const ACollection: IJclPtrCollection): Boolean;
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

procedure TJclPtrBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclPtrBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclPtrBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclPtrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

//=== { TJclPtrBinaryTreeIterator } ===========================================================

constructor TJclPtrBinaryTreeIterator.Create(const AOwnTree: IJclPtrCollection; ACursor: TJclPtrBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclPtrEqualityComparer;
end;

function TJclPtrBinaryTreeIterator.Add(APtr: Pointer): Boolean;
begin
  Result := FOwnTree.Add(APtr);
end;

function TJclPtrBinaryTreeIterator.AddChild(APtr: Pointer): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclPtrBinaryTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclPtrBinaryTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclPtrBinaryTreeIterator then
  begin
    ADest := TJclPtrBinaryTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclPtrBinaryTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrBinaryTreeIterator.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclPtrBinaryTreeIterator.DeleteChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclPtrBinaryTreeIterator.Extract;
var
  OldCursor: TJclPtrBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Extract(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrBinaryTreeIterator.ExtractChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclPtrBinaryTreeIterator.ExtractChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclPtrBinaryTreeIterator.GetChild(Index: Integer): Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
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

function TJclPtrBinaryTreeIterator.GetPointer: Pointer;
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

function TJclPtrBinaryTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTreeIterator.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTreeIterator.HasNext: Boolean;
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

function TJclPtrBinaryTreeIterator.HasParent: Boolean;
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

function TJclPtrBinaryTreeIterator.HasPrevious: Boolean;
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

function TJclPtrBinaryTreeIterator.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTreeIterator.IndexOfChild(APtr: Pointer): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, APtr) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, APtr) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, APtr) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTreeIterator.Insert(APtr: Pointer): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclPtrBinaryTreeIterator.InsertChild(Index: Integer; APtr: Pointer): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclPtrBinaryTreeIterator.IteratorEquals(const AIterator: IJclPtrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclPtrBinaryTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclPtrBinaryTreeIterator then
  begin
    ItrObj := TJclPtrBinaryTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TJclPtrBinaryTreeIterator.Left: Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FCursor <> nil then
      FCursor := FCursor.Left;
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

{$IFDEF SUPPORTS_FOR_IN}
function TJclPtrBinaryTreeIterator.MoveNext: Boolean;
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

function TJclPtrBinaryTreeIterator.Next: Pointer;
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

function TJclPtrBinaryTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclPtrBinaryTreeIterator.Parent: Pointer;
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

function TJclPtrBinaryTreeIterator.Previous: Pointer;
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

function TJclPtrBinaryTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclPtrBinaryTreeIterator.Remove;
var
  OldCursor: TJclPtrBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrBinaryTreeIterator.Reset;
var
  NewCursor: TJclPtrBinaryNode;
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

function TJclPtrBinaryTreeIterator.Right: Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FCursor <> nil then
      FCursor := FCursor.Right;
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

procedure TJclPtrBinaryTreeIterator.SetChild(Index: Integer; APtr: Pointer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclPtrBinaryTreeIterator.SetPointer(APtr: Pointer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TJclPreOrderPtrBinaryTreeIterator } ===================================================

function TJclPreOrderPtrBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderPtrBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderPtrBinaryTreeIterator.GetNextCursor: TJclPtrBinaryNode;
var
  LastRet: TJclPtrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TJclPreOrderPtrBinaryTreeIterator.GetPreviousCursor: TJclPtrBinaryNode;
var
  LastRet: TJclPtrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TJclInOrderPtrBinaryTreeIterator } ====================================================

function TJclInOrderPtrBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclInOrderPtrBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclInOrderPtrBinaryTreeIterator.GetNextCursor: TJclPtrBinaryNode;
var
  LastRet: TJclPtrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TJclInOrderPtrBinaryTreeIterator.GetPreviousCursor: TJclPtrBinaryNode;
var
  LastRet: TJclPtrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TJclPostOrderPtrBinaryTreeIterator } ==================================================

function TJclPostOrderPtrBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderPtrBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderPtrBinaryTreeIterator.GetNextCursor: TJclPtrBinaryNode;
var
  LastRet: TJclPtrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TJclPostOrderPtrBinaryTreeIterator.GetPreviousCursor: TJclPtrBinaryNode;
var
  LastRet: TJclPtrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TJclBinaryTree } =================================================

constructor TJclBinaryTree.Create(ACompare: TCompare; AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclBinaryTree.Add(AObject: TObject): Boolean;
var
  NewNode, Current, Save: TJclBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AObject, nil) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AObject, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclBinaryNode.Create;
        NewNode.Value := AObject;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.AddAll(const ACollection: IJclCollection): Boolean;
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

procedure TJclBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclBinaryTree;
  ACollection: IJclCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclBinaryTree then
  begin
    ADest := TJclBinaryTree(Dest);
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

procedure TJclBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclBinaryTree then
    TJclBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

function TJclBinaryTree.BuildTree(const LeafArray: array of TJclBinaryNode; Left, Right: Integer; Parent: TJclBinaryNode;
  Offset: Integer): TJclBinaryNode;
var
  Middle: Integer;
begin
  Middle := (Left + Right + Offset) shr 1;
  Result := LeafArray[Middle];
  Result.Parent := Parent;
  if Middle > Left then
    Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
  else
    Result.Left := nil;
  if Middle < Right then
    Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
  else
    Result.Right := nil;
end;

procedure TJclBinaryTree.Clear;
var
  Current, Parent: TJclBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeObject(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.CloneNode(Node, Parent: TJclBinaryNode): TJclBinaryNode;
begin
  Result := TJclBinaryNode.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  if Node.Left <> nil then
    Result.Left := CloneNode(Node.Left, Result); // recursive call
  if Node.Right <> nil then
    Result.Right := CloneNode(Node.Right, Result); // recursive call
end;

function TJclBinaryTree.CollectionEquals(const ACollection: IJclCollection): Boolean;
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

function TJclBinaryTree.Contains(AObject: TObject): Boolean;
var
  Comp: Integer;
  Current: TJclBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AObject);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.ContainsAll(const ACollection: IJclCollection): Boolean;
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

function TJclBinaryTree.Extract(AObject: TObject): Boolean;
var
  Current, Successor: TJclBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AObject in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AObject, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      Current.Value := nil;
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.ExtractAll(const ACollection: IJclCollection): Boolean;
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

function TJclBinaryTree.First: IJclIterator;
var
  Start: TJclBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderBinaryTreeIterator.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TJclInOrderBinaryTreeIterator.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TJclPostOrderBinaryTreeIterator.Create(Self, Start, False, isFirst);
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
function TJclBinaryTree.GetEnumerator: IJclIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclBinaryTree.GetRoot: IJclTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TJclPreOrderBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TJclInOrderBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TJclPostOrderBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
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

function TJclBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclBinaryTree.Last: IJclIterator;
var
  Start: TJclBinaryNode;
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
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TJclPreOrderBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TJclInOrderBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TJclPostOrderBinaryTreeIterator.Create(Self, Start, False, isLast);
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

procedure TJclBinaryTree.Pack;
var
  LeafArray: array of TJclBinaryNode;
  ANode, BNode: TJclBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.Remove(AObject: TObject): Boolean;
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

function TJclBinaryTree.RemoveAll(const ACollection: IJclCollection): Boolean;
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

function TJclBinaryTree.RetainAll(const ACollection: IJclCollection): Boolean;
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

procedure TJclBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

function TJclBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTree.Create(Compare, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclBinaryTreeIterator } ===========================================================

constructor TJclBinaryTreeIterator.Create(const AOwnTree: IJclCollection; ACursor: TJclBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclEqualityComparer;
end;

function TJclBinaryTreeIterator.Add(AObject: TObject): Boolean;
begin
  Result := FOwnTree.Add(AObject);
end;

function TJclBinaryTreeIterator.AddChild(AObject: TObject): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTreeIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclBinaryTreeIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclBinaryTreeIterator then
  begin
    ADest := TJclBinaryTreeIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclBinaryTreeIterator.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTreeIterator.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTreeIterator.DeleteChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTreeIterator.Extract;
var
  OldCursor: TJclBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Extract(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTreeIterator.ExtractChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTreeIterator.ExtractChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclBinaryTreeIterator.GetChild(Index: Integer): TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
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

function TJclBinaryTreeIterator.GetObject: TObject;
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

function TJclBinaryTreeIterator.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTreeIterator.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTreeIterator.HasNext: Boolean;
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

function TJclBinaryTreeIterator.HasParent: Boolean;
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

function TJclBinaryTreeIterator.HasPrevious: Boolean;
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

function TJclBinaryTreeIterator.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTreeIterator.IndexOfChild(AObject: TObject): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AObject) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AObject) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AObject) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTreeIterator.Insert(AObject: TObject): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclBinaryTreeIterator.InsertChild(Index: Integer; AObject: TObject): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclBinaryTreeIterator.IteratorEquals(const AIterator: IJclIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclBinaryTreeIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclBinaryTreeIterator then
  begin
    ItrObj := TJclBinaryTreeIterator(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TJclBinaryTreeIterator.Left: TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FCursor <> nil then
      FCursor := FCursor.Left;
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

{$IFDEF SUPPORTS_FOR_IN}
function TJclBinaryTreeIterator.MoveNext: Boolean;
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

function TJclBinaryTreeIterator.Next: TObject;
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

function TJclBinaryTreeIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclBinaryTreeIterator.Parent: TObject;
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

function TJclBinaryTreeIterator.Previous: TObject;
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

function TJclBinaryTreeIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTreeIterator.Remove;
var
  OldCursor: TJclBinaryNode;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTreeIterator.Reset;
var
  NewCursor: TJclBinaryNode;
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

function TJclBinaryTreeIterator.Right: TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FCursor <> nil then
      FCursor := FCursor.Right;
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

procedure TJclBinaryTreeIterator.SetChild(Index: Integer; AObject: TObject);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTreeIterator.SetObject(AObject: TObject);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TJclPreOrderBinaryTreeIterator } ===================================================

function TJclPreOrderBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderBinaryTreeIterator.GetNextCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TJclPreOrderBinaryTreeIterator.GetPreviousCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TJclInOrderBinaryTreeIterator } ====================================================

function TJclInOrderBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclInOrderBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclInOrderBinaryTreeIterator.GetNextCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TJclInOrderBinaryTreeIterator.GetPreviousCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TJclPostOrderBinaryTreeIterator } ==================================================

function TJclPostOrderBinaryTreeIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderBinaryTreeIterator.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderBinaryTreeIterator.GetNextCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TJclPostOrderBinaryTreeIterator.GetPreviousCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;



{$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

//=== { TJclBinaryTree<T> } =================================================

constructor TJclBinaryTree<T>.Create(AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
end;

destructor TJclBinaryTree<T>.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclBinaryTree<T>.Add(const AItem: T): Boolean;
var
  NewNode, Current, Save: TJclBinaryNode<T>;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AItem, Default(T)) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AItem, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclBinaryNode<T>.Create;
        NewNode.Value := AItem;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
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

procedure TJclBinaryTree<T>.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclBinaryTree<T>;
  ACollection: IJclCollection<T>;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclBinaryTree<T> then
  begin
    ADest := TJclBinaryTree<T>(Dest);
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

procedure TJclBinaryTree<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclBinaryTree<T> then
    TJclBinaryTree<T>(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclBinaryTree<T>.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

function TJclBinaryTree<T>.BuildTree(const LeafArray: array of TJclBinaryNode<T>; Left, Right: Integer; Parent: TJclBinaryNode<T>;
  Offset: Integer): TJclBinaryNode<T>;
var
  Middle: Integer;
begin
  Middle := (Left + Right + Offset) shr 1;
  Result := LeafArray[Middle];
  Result.Parent := Parent;
  if Middle > Left then
    Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
  else
    Result.Left := nil;
  if Middle < Right then
    Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
  else
    Result.Right := nil;
end;

procedure TJclBinaryTree<T>.Clear;
var
  Current, Parent: TJclBinaryNode<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeItem(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.CloneNode(Node, Parent: TJclBinaryNode<T>): TJclBinaryNode<T>;
begin
  Result := TJclBinaryNode<T>.Create;
  Result.Value := Node.Value;
  Result.Parent := Parent;
  if Node.Left <> nil then
    Result.Left := CloneNode(Node.Left, Result); // recursive call
  if Node.Right <> nil then
    Result.Right := CloneNode(Node.Right, Result); // recursive call
end;

function TJclBinaryTree<T>.CollectionEquals(const ACollection: IJclCollection<T>): Boolean;
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

function TJclBinaryTree<T>.Contains(const AItem: T): Boolean;
var
  Comp: Integer;
  Current: TJclBinaryNode<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AItem);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.ContainsAll(const ACollection: IJclCollection<T>): Boolean;
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

function TJclBinaryTree<T>.Extract(const AItem: T): Boolean;
var
  Current, Successor: TJclBinaryNode<T>;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AItem in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AItem, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      Current.Value := Default(T);
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.ExtractAll(const ACollection: IJclCollection<T>): Boolean;
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

function TJclBinaryTree<T>.First: IJclIterator<T>;
var
  Start: TJclBinaryNode<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderBinaryTreeIterator.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderBinaryTreeIterator.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TPostOrderBinaryTreeIterator.Create(Self, Start, False, isFirst);
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
function TJclBinaryTree<T>.GetEnumerator: IJclIterator<T>;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclBinaryTree<T>.GetRoot: IJclTreeIterator<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TInOrderBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TPostOrderBinaryTreeIterator.Create(Self, FRoot, False, isRoot);
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

function TJclBinaryTree<T>.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclBinaryTree<T>.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclBinaryTree<T>.Last: IJclIterator<T>;
var
  Start: TJclBinaryNode<T>;
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
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TPreOrderBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderBinaryTreeIterator.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TPostOrderBinaryTreeIterator.Create(Self, Start, False, isLast);
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

procedure TJclBinaryTree<T>.Pack;
var
  LeafArray: array of TJclBinaryNode<T>;
  ANode, BNode: TJclBinaryNode<T>;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.Remove(const AItem: T): Boolean;
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

function TJclBinaryTree<T>.RemoveAll(const ACollection: IJclCollection<T>): Boolean;
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

function TJclBinaryTree<T>.RetainAll(const ACollection: IJclCollection<T>): Boolean;
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

procedure TJclBinaryTree<T>.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTree<T>.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclBinaryTree<T>.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclBinaryTreeIterator<T> } ===========================================================

constructor TJclBinaryTreeIterator<T>.Create(const AOwnTree: IJclCollection<T>; ACursor: TJclBinaryNode<T>; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclEqualityComparer<T>;
end;

function TJclBinaryTreeIterator<T>.Add(const AItem: T): Boolean;
begin
  Result := FOwnTree.Add(AItem);
end;

function TJclBinaryTreeIterator<T>.AddChild(const AItem: T): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTreeIterator<T>.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclBinaryTreeIterator<T>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclBinaryTreeIterator<T> then
  begin
    ADest := TJclBinaryTreeIterator<T>(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclBinaryTreeIterator<T>.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTreeIterator<T>.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTreeIterator<T>.DeleteChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTreeIterator<T>.Extract;
var
  OldCursor: TJclBinaryNode<T>;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Extract(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTreeIterator<T>.ExtractChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTreeIterator<T>.ExtractChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclBinaryTreeIterator<T>.GetChild(Index: Integer): T;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
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

function TJclBinaryTreeIterator<T>.GetItem: T;
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

function TJclBinaryTreeIterator<T>.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTreeIterator<T>.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTreeIterator<T>.HasNext: Boolean;
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

function TJclBinaryTreeIterator<T>.HasParent: Boolean;
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

function TJclBinaryTreeIterator<T>.HasPrevious: Boolean;
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

function TJclBinaryTreeIterator<T>.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTreeIterator<T>.IndexOfChild(const AItem: T): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AItem) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AItem) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AItem) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTreeIterator<T>.Insert(const AItem: T): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclBinaryTreeIterator<T>.InsertChild(Index: Integer; const AItem: T): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclBinaryTreeIterator<T>.IteratorEquals(const AIterator: IJclIterator<T>): Boolean;
var
  Obj: TObject;
  ItrObj: TJclBinaryTreeIterator<T>;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclBinaryTreeIterator<T> then
  begin
    ItrObj := TJclBinaryTreeIterator<T>(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TJclBinaryTreeIterator<T>.Left: T;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if FCursor <> nil then
      FCursor := FCursor.Left;
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

{$IFDEF SUPPORTS_FOR_IN}
function TJclBinaryTreeIterator<T>.MoveNext: Boolean;
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

function TJclBinaryTreeIterator<T>.Next: T;
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

function TJclBinaryTreeIterator<T>.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclBinaryTreeIterator<T>.Parent: T;
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

function TJclBinaryTreeIterator<T>.Previous: T;
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

function TJclBinaryTreeIterator<T>.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTreeIterator<T>.Remove;
var
  OldCursor: TJclBinaryNode<T>;
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
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTreeIterator<T>.Reset;
var
  NewCursor: TJclBinaryNode<T>;
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

function TJclBinaryTreeIterator<T>.Right: T;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if FCursor <> nil then
      FCursor := FCursor.Right;
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

procedure TJclBinaryTreeIterator<T>.SetChild(Index: Integer; const AItem: T);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTreeIterator<T>.SetItem(const AItem: T);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TJclPreOrderBinaryTreeIterator<T> } ===================================================

function TJclPreOrderBinaryTreeIterator<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPreOrderBinaryTreeIterator<T>.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPreOrderBinaryTreeIterator<T>.GetNextCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TJclPreOrderBinaryTreeIterator<T>.GetPreviousCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TJclInOrderBinaryTreeIterator<T> } ====================================================

function TJclInOrderBinaryTreeIterator<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclInOrderBinaryTreeIterator<T>.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclInOrderBinaryTreeIterator<T>.GetNextCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TJclInOrderBinaryTreeIterator<T>.GetPreviousCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TJclPostOrderBinaryTreeIterator<T> } ==================================================

function TJclPostOrderBinaryTreeIterator<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPostOrderBinaryTreeIterator<T>.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TJclPostOrderBinaryTreeIterator<T>.GetNextCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TJclPostOrderBinaryTreeIterator<T>.GetPreviousCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TJclBinaryTreeE<T> } =================================================

constructor TJclBinaryTreeE<T>.Create(const AComparer: IJclComparer<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FComparer := AComparer;
end;

procedure TJclBinaryTreeE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclBinaryTreeE<T> then
    TJclBinaryTreeE<T>(Dest).FComparer := FComparer;
end;

function TJclBinaryTreeE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTreeE<T>.Create(Comparer, False);
  AssignPropertiesTo(Result);
end;

function TJclBinaryTreeE<T>.ItemsCompare(const A, B: T): Integer;
begin
  if Comparer <> nil then
    Result := Comparer.Compare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclBinaryTreeE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Comparer <> nil then
    Result := Comparer.Compare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclBinaryTreeF<T> } =================================================

constructor TJclBinaryTreeF<T>.Create(ACompare: TCompare<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  SetCompare(ACompare);
end;

function TJclBinaryTreeF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTreeF<T>.Create(Compare, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclBinaryTreeI<T> } =================================================

function TJclBinaryTreeI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTreeI<T>.Create(False);
  AssignPropertiesTo(Result);
end;

function TJclBinaryTreeI<T>.ItemsCompare(const A, B: T): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := A.CompareTo(B);
end;

function TJclBinaryTreeI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := A.CompareTo(B) = 0;
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

