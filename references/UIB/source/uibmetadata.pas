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
(* Contributor: Pierre Yager                                                    *)
(*                                                                              *)
(********************************************************************************)

{$I uib.inc}

unit uibmetadata;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, uibase, uiblib, uib, uibconst, uibkeywords, uibavl;

type
  TTriggerPrefix = (tpBefore, tpAfter);
  TTriggerSuffix = (tsInsert, tsUpdate, tsDelete);
  TTriggerSuffixes = set of TTriggerSuffix;
  TIndexOrder = (ioDescending, ioAscending);
  TUpdateRule = (urRestrict, urCascade, urSetNull, urNoAction, urSetDefault);
  TTableFieldInfo = (fiPrimary, fiForeign, fiIndice, fiUnique);
  TTableFieldInfos = set of TTableFieldInfo;

  {$HPPEMIT 'namespace Uibmetadata'}
  {$HPPEMIT '{'}
  {$HPPEMIT 'class DELPHICLASS TMetaNode;'}
  (*$HPPEMIT '}'*)
  {$HPPEMIT ''}

  // indentation = inherit
  TMetaNodeType =
   (
    MetaNode,
      MetaDatabase,
      MetaException,
      MetaGenerator,
      MetaCheck,
      MetaTrigger,
      MetaUDF,
      MetaView,
      MetaProcedure,
      MetaRole,
      MetaTable,
      MetaBaseField,         { Characteristics, Charset, Collation, Default ... }
        MetaUDFField,
        MetaField,
          MetaProcInField,
          MetaProcOutField,
          MetaTableField,    { Nullable, Domain, Check, Computed By ...}
            MetaDomain,
      MetaConstraint,
        MetaForeign,
        MetaIndex,
        MetaPrimary,
        MetaUnique,
      MetaGrant,
        MetaRoleGrant,
        MetaTableGrant,
        MetaFieldGrant,
        MetaProcedureGrant,
      MetaGrantee,
        MetaUserGrantee,
        MetaRoleGrantee,
        MetaProcedureGrantee,
        MetaTriggerGrantee,
        MetaViewGrantee
   );

  // forward declarations
  TMetaNode = class;
  TMetaDomain = class;
  TMetaTable = class;
  TMetaView = class;
  TMetaRoleGrant = class;
  TMetaProcedureGrant = class;
  TMetaTableGrant = class;
  TMetaFieldGrant = class;
  TMetaDatabase = class;

  TMetaNodeClass = class of TMetaNode;

  TNodeItem = record
    Childs: TList;
    ClassID: TMetaNodeClass;
  end;

  TDDLOption = (ddlFull);
  TDDLOptions = set of TDDLOption;

  TMetaNodeArray = array of TMetaNode;

  TMetaNode = class(TObject)
  private
    FName: string;
    FOwner: TMetaNode;
    FNodeItems: array of TNodeItem;
    FNodeItemsCount: Integer;
    FData: Pointer;
    FDependents: TMetaNodeArray;
    FDependedOn: TMetaNodeArray;
    function GetItems(const ClassIndex, Index: Integer): TMetaNode;
    procedure AddClass(ClassID: TMetaNodeClass);
    procedure CheckTransaction(Transaction: TUIBTransaction);
    procedure SaveNode(Stream: TStringStream; OID: Integer; options: TDDLOptions; Separator: string = NewLine);
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    function GetAsDDL: string;
    function GetAsDDLNode: string;
    function GetAsFullDDL: string;
    function GetAsFullDDLNode: string;
    function GetDependedOn(const Index: Integer): TMetaNode;
    function GetDependedOnCount: Integer;
    function GetDependent(const Index: Integer): TMetaNode;
    function GetDependentCount: Integer;
  protected
    function GetName: String; virtual;
  public
    constructor Create(AOwner: TMetaNode; ClassIndex: Integer); virtual;
    constructor CreateFromStream(AOwner: TMetaNode; ClassIndex: Integer; Stream: TStream); virtual;
    destructor Destroy; override;
    class function NodeClass: string; virtual;
    class function NodeType: TMetaNodeType; virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); virtual;
    procedure SaveToDDL(Stream: TStringStream; options: TDDLOptions); virtual;
    procedure RegisterDependent(OtherNode: TMetaNode);
    procedure RegisterDependedOn(OtherNode: TMetaNode);
    function GetNodes(const Index: Integer): TNodeItem;
    function GetDatabase: TMetaDatabase;
    function MetaQuote(const str: string): string;    
    property Name: string read GetName;
    property AsDDL: string read GetAsDDL;
    property AsDDLNode: string read GetAsDDLNode;
    property AsFullDDL: string read GetAsFullDDL;
    property AsFullDDLNode: string read GetAsFullDDLNode;
    property NodeCount: Integer read FNodeItemsCount;
    property Nodes[const Index: Integer]: TNodeItem read GetNodes;
    property Parent: TMetaNode read FOwner;
    property Data: Pointer read FData write FData;
    property DependentCount: Integer read GetDependentCount;
    property Dependent[const Index: Integer]: TMetaNode read GetDependent;
    property DependedOnCount: Integer read GetDependedOnCount;
    property DependedOn[const Index: Integer]: TMetaNode read GetDependedOn;
  end;

  TMetaGenerator = class(TMetaNode)
  private
    FValue: Integer;
    procedure LoadFromDataBase(Transaction: TUIBTransaction; const Name: string);
    procedure LoadFromStream(Stream: TStream); override;
    function GetAsAlterDDL: string;
    function GetAsCreateDLL: string;
  public
    procedure SaveToCreateDDLNode(Stream: TStringStream);
    procedure SaveToAlterDDLNode(Stream: TStringStream);
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToStream(Stream: TStream); override;
    property Value: Integer read FValue;
    property AsCreateDLL: string read GetAsCreateDLL;
    property AsAlterDDL: string read GetAsAlterDDL;
  end;

  TMetaBaseField = class(TMetaNode)
  private
    FScale: Word;
    FLength: Smallint;
    FPrecision: Smallint;
    FFieldType: TUIBFieldType;
    FCharSet: string;
    FCollation: string;
    FSegmentLength: Smallint;
    FSubType: Smallint;
    FBytesPerCharacter: Smallint;
    FDefaultValue: string;
    procedure LoadFromQuery(QField, QCharset, QArrayDim: TUIBStatement; DefaultCharset: TCharacterSet); virtual;
    procedure LoadFromStream(Stream: TStream); override;
    function GetShortFieldType: string; virtual;
  protected
    property SegmentLength: Smallint read FSegmentLength;
    property DefaultValue: string read FDefaultValue;
  public
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToStream(Stream: TStream); override;
    property Scale: Word read FScale;
    property Length: Smallint read FLength;
    property Precision: Smallint read FPrecision;
    property FieldType: TUIBFieldType read FFieldType;
    property CharSet: string read FCharSet;
    property SubType: Smallint read FSubType;
    property BytesPerCharacter: Smallint read FBytesPerCharacter;
    property ShortFieldType: string read GetShortFieldType;
  end;

  TMetaField = class(TMetaBaseField)
  private
    procedure LoadFromQuery(Q, C, A: TUIBStatement; DefaultCharset: TCharacterSet); override;
  public
    class function NodeType: TMetaNodeType; override;
    procedure SaveToDDL(Stream: TStringStream; options: TDDLOptions); override;
    property SegmentLength;
    property DefaultValue;
  end;

  TMetaProcInField = class(TMetaField)
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
  end;

  TMetaProcOutField = class(TMetaField)
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
  end;

  PArrayBound = ^TArrayBound;
  TArrayBound = record
    LowerBound: Integer;
    HigherBound: Integer;
  end;

  TMetaTableField = class(TMetaField)
  private
    FNotNull: Boolean;
    FDomain: Integer;
    FInfos: TTableFieldInfos;
    FValidationSource: string;
    FComputedSource: string;
    FArrayBounds: array of TArrayBound;
    procedure LoadFromQuery(Q, C, A: TUIBStatement; DefaultCharset: TCharacterSet); override;
    procedure LoadFromStream(Stream: TStream); override;
    function GetDomain: TMetaDomain;
    function GetArrayBounds(const index: Integer): TArrayBound;
    function GetArrayBoundsCount: Integer;
    function GetShortFieldType: string; override;
  public
    class function NodeType: TMetaNodeType; override;
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    procedure SaveToStream(Stream: TStream); override;
    property NotNull: Boolean read FNotNull;
    property Domain: TMetaDomain read GetDomain;
    property FieldInfos: TTableFieldInfos read FInfos;
    property ComputedSource: string read FComputedSource;
    property ValidationSource: string read FValidationSource;
    property ArrayBoundsCount: Integer read GetArrayBoundsCount;
    property ArrayBounds[const index: Integer]: TArrayBound read GetArrayBounds;
  end;

  TMetaDomain = class(TMetaTableField)
  protected
    property Domain; // hidden
    property ComputedSource; // hidden
  public
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    procedure SaveToDDL(Stream: TStringStream; options: TDDLOptions); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
  end;

  TMetaConstraint = class(TMetaNode)
  private
    FOrder: TIndexOrder;
    FFields: array of Integer;
    function GetFields(const Index: Word): TMetaTableField;
    function GetFieldsCount: Word;
    procedure LoadFromStream(Stream: TStream); override;
    function GetAsDropDDL: string;
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToStream(Stream: TStream); override;
    property Fields[const Index: Word]: TMetaTableField read GetFields;
    property FieldsCount: Word read GetFieldsCount;
    property Order: TIndexOrder read FOrder;
    property AsDropDDL: string read GetAsDropDDL;
  end;

  TMetaPrimary = class(TMetaConstraint)
  private
    FIndexName: string;
    procedure LoadFromQuery(Q: TUIBStatement);
    procedure LoadFromStream(Stream: TStream); override;
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    procedure SaveToStream(Stream: TStream); override;
    property IndexName: string read FIndexName;
  end;

  TMetaUnique = class(TMetaConstraint)
  private
    FIndexName: string;
    procedure LoadFromStream(Stream: TStream); override;
    function GetAsAlterToActiveDDL: string;
    function GetAsAlterToInactiveDDL: string;
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToDDL(Stream: TStringStream; options: TDDLOptions); override;
    procedure SaveToStream(Stream: TStream); override;
    property IndexName: string read FIndexName;
    property AsAlterToActiveDDL: string read GetAsAlterToActiveDDL;
    property AsAlterToInactiveDDL: string read GetAsAlterToInactiveDDL;
  end;

  TMetaForeign = class(TMetaConstraint)
  private
    FIndexName: string;
    FForTable: Integer;
    FForFields: array of Integer;
    FOnDelete: TUpdateRule;
    FOnUpdate: TUpdateRule;
    function GetForFields(const Index: Word): TMetaTableField;
    function GetForFieldsCount: Word;
    function GetForTable: TMetaTable;
    procedure LoadFromStream(Stream: TStream); override;
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    property ForTable: TMetaTable read GetForTable;
    property ForFields[const Index: Word]: TMetaTableField read GetForFields;
    property ForFieldsCount: Word read GetForFieldsCount;
    property OnDelete: TUpdateRule read FOnDelete;
    property OnUpdate: TUpdateRule read FOnUpdate;
    property IndexName: string read FIndexName;
  end;

  TMetaIndex = class(TMetaConstraint)
  private
    FUnique: Boolean;
    FActive: Boolean;
  {$IFDEF FB21_UP}
    FComputedSource: string;
  {$ENDIF}
    procedure LoadFromStream(Stream: TStream); override;
    function GetAsAlterDDL: string;
    function GetAsAlterToActiveDDL: string;
    function GetAsAlterToInactiveDDL: string;
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    procedure SaveToStream(Stream: TStream); override;
    property Unique: Boolean read FUnique;
    property Active: Boolean read FActive;
    property AsAlterDDL: string read GetAsAlterDDL;
    property AsAlterToActiveDDL: string read GetAsAlterToActiveDDL;
    property AsAlterToInactiveDDL: string read GetAsAlterToInactiveDDL;
  end;

  TMetaCheck = class(TMetaNode)
  private
    FConstraint: string;
    procedure LoadFromStream(Stream: TStream); override;
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    procedure SaveToStream(Stream: TStream); override;
    property Constraint: string read FConstraint;
  end;

  TMetaTrigger = class(TMetaNode)
  private
    FPrefix: TTriggerPrefix;
    FSuffix: TTriggerSuffixes;
    FPosition: Smallint;
    FActive: Boolean;
    FSource: string;
    class function DecodePrefix(Value: Integer): TTriggerPrefix;
    class function DecodeSuffixes(Value: Integer): TTriggerSuffixes;
    procedure LoadFromQuery(Q: TUIBStatement);
    procedure LoadFromStream(Stream: TStream); override;
    function GetAsAlterDDL: string;
    function GetAsAlterToActiveDDL: string;
    function GetAsAlterToInactiveDDL: string;
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    procedure SaveToAlterDDL(Stream: TStringStream);
    procedure SaveToAlterToActiveDDL(Stream: TStringStream);
    procedure SaveToAlterToInactiveDDL(Stream: TStringStream);
    property Prefix: TTriggerPrefix read FPrefix;
    property Suffix: TTriggerSuffixes read FSuffix;
    property Position: Smallint read FPosition;
    property Active: Boolean read FActive;
    property Source: string read FSource;
    property AsAlterDDL: string read GetAsAlterDDL;
    property AsAlterToActiveDDL: string read GetAsAlterToActiveDDL;
    property AsAlterToInactiveDDL: string read GetAsAlterToInactiveDDL;
  end;

  TMetaBaseTable = class(TMetaNode)
  private
    function GetTableGrants(const Index: Integer): TMetaTableGrant;
    function GetTableGrantsCount: Integer;
    function GetFieldsGrants(const Index: Integer): TMetaFieldGrant;
    function GetFieldsGrantsCount: Integer;
  protected
    FGrantsClassIndex: Integer;
    FFieldsGrantsClassIndex: Integer;
    procedure LoadGrantsFromQuery(G: TUIBStatement);
    procedure LoadFieldsGrantsFromQuery(G: TUIBStatement);
    function FindGrant(const Privileges: TTablePrivileges; Option: Boolean): TMetaTableGrant;
    function FindFieldGrant(const Privilege: TFieldPrivilege; Option: Boolean; Fields: TStringList): TMetaFieldGrant;
  public
    property Grants[const Index: Integer]: TMetaTableGrant read GetTableGrants;
    property GrantsCount: Integer read GetTableGrantsCount;

    property FieldsGrants[const Index: Integer]: TMetaFieldGrant read GetFieldsGrants;
    property FieldsGrantsCount: Integer read GetFieldsGrantsCount;
  end;

  TMetaTable = class(TMetaBaseTable)
  private
    function GetFields(const Index: Integer): TMetaTableField;
    function GetFieldsCount: Integer;
    procedure LoadFromDataBase(QNames, QFields, QCharset, QPrimary,
      QIndex, QCheck, QTrigger, QArrayDim, QGrants, QFieldGrants: TUIBStatement;
      OIDs: TOIDTables; DefaultCharset: TCharacterSet);
    function FindFieldIndex(const Name: string): Integer;
    function GetUniques(const Index: Integer): TMetaUnique;
    function GetUniquesCount: Integer;
    function GetPrimary(const Index: Integer): TMetaPrimary;
    function GetPrimaryCount: Integer;
    function GetIndices(const Index: Integer): TMetaIndex;
    function GetIndicesCount: Integer;
    function GetForeign(const Index: Integer): TMetaForeign;
    function GetForeignCount: Integer;
    function GetChecks(const Index: Integer): TMetaCheck;
    function GetChecksCount: Integer;
    function GetTriggers(const Index: Integer): TMetaTrigger;
    function GetTriggersCount: Integer;
    procedure LoadFromStream(Stream: TStream); override;
  public
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    function FindFieldName(const Name: string): TMetaTableField;
    constructor Create(AOwner: TMetaNode; ClassIndex: Integer); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToDDL(Stream: TStringStream; options: TDDLOptions); override;

    property Fields[const Index: Integer]: TMetaTableField read GetFields;
    property FieldsCount: Integer read GetFieldsCount;

    property Primary[const Index: Integer]: TMetaPrimary read GetPrimary;
    property PrimaryCount: Integer read GetPrimaryCount; // 0 or 1

    property Uniques[const Index: Integer]: TMetaUnique read GetUniques;
    property UniquesCount: Integer read GetUniquesCount;

    property Indices[const Index: Integer]: TMetaIndex read GetIndices;
    property IndicesCount: Integer read GetIndicesCount;

    property Foreign[const Index: Integer]: TMetaForeign read GetForeign;
    property ForeignCount: Integer read GetForeignCount;

    property Checks[const Index: Integer]: TMetaCheck read GetChecks;
    property ChecksCount: Integer read GetChecksCount;

    property Triggers[const Index: Integer]: TMetaTrigger read GetTriggers;
    property TriggersCount: Integer read GetTriggersCount;
  end;

  TMetaView = class(TMetaBaseTable)
  private
    FSource: string;
    function GetFields(const Index: Integer): TMetaField;
    function GetFieldsCount: Integer;
    function GetTriggers(const Index: Integer): TMetaTrigger;
    function GetTriggersCount: Integer;
    procedure LoadFromDataBase(QName, QFields, QTriggers,
      QCharset, QArrayDim, QGrants, QFieldGrants: TUIBStatement; OIDs: TOIDViews;
      DefaultCharset: TCharacterSet);
    procedure LoadFromStream(Stream: TStream); override;
    function GetAsDropDDL: string;
  public
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    constructor Create(AOwner: TMetaNode; ClassIndex: Integer); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToDDL(Stream: TStringStream; options: TDDLOptions); override;
    property Source: string read FSource;
    property Fields[const Index: Integer]: TMetaField read GetFields;
    property FieldsCount: Integer read GetFieldsCount;
    property Triggers[const Index: Integer]: TMetaTrigger read GetTriggers;
    property TriggersCount: Integer read GetTriggersCount;
    property AsDropDDL: string read GetAsDropDDL;
  end;

  TMetaProcedure = class(TMetaNode)
  private
    FSource: string;
    procedure LoadFromQuery(QNames, QFields, QCharset, QArrayDim, QGrants: TUIBStatement;
      OIDs: TOIDProcedures; DefaultCharset: TCharacterSet);
    procedure LoadGrantsFromQuery(QGrants: TUIBStatement);
    function GetInputFields(const Index: Integer): TMetaProcInField;
    function GetInputFieldsCount: Integer;
    function GetOutputFields(const Index: Integer): TMetaProcOutField;
    function GetOutputFieldsCount: Integer;
    procedure LoadFromStream(Stream: TStream); override;
    procedure InternalSaveToDDL(Stream: TStringStream; Operation: string; options: TDDLOptions);
    procedure SaveToCreateEmptyDDL(Stream: TStringStream; options: TDDLOptions);
    function GetAsAlterDDL: string;
    function GetAsCreateEmptyDDL: string;
    function GetProcedureGrants(const Index: Integer): TMetaProcedureGrant;
    function GetProcedureGrantsCount: Integer;
    function GetAsAlterToEmptyDDL: string;
  protected
    function FindGrant(Option: Boolean): TMetaProcedureGrant;
  public
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    constructor Create(AOwner: TMetaNode; ClassIndex: Integer); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToAlterDDL(Stream: TStringStream; options: TDDLOptions);

    property Source: string read FSource;
    property AsCreateEmptyDDL: string read GetAsCreateEmptyDDL;
    property AsAlterDDL: string read GetAsAlterDDL;
    property AsAlterToEmptyDLL: string read GetAsAlterToEmptyDDL;

    property InputFields[const Index: Integer]: TMetaProcInField read GetInputFields;
    property InputFieldsCount: Integer read GetInputFieldsCount;

    property OutputFields[const Index: Integer]: TMetaProcOutField read GetOutputFields;
    property OutputFieldsCount: Integer read GetOutputFieldsCount;

    property Grants[const Index: Integer]: TMetaProcedureGrant read GetProcedureGrants;
    property GrantsCount: Integer read GetProcedureGrantsCount;
  end;

  TMetaException = class(TMetaNode)
  private
    FMessage: string;
    FNumber: Integer;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromQuery(QName: TUIBStatement);
  public
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToStream(Stream: TStream); override;
    property Message: string read FMessage;
    property Number: Integer read FNumber;
  end;

  TMetaUDFField = class(TMetaBaseField)
  private
    FPosition: Smallint;
    FMechanism: Smallint;
    procedure LoadFromQuery(QField, QCharset, QArrayDim: TUIBStatement; DefaultCharset: TCharacterSet); override;
    procedure LoadFromStream(Stream: TStream); override;
  public
    class function NodeType: TMetaNodeType; override;
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    procedure SaveToStream(Stream: TStream); override;
    property Position: Smallint read FPosition;
    property Mechanism: Smallint read FMechanism;
  end;

  TMetaUDF = class(TMetaNode)
  private
    FModule: string;
    FEntry: string;
    FReturn: Smallint;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromQuery(QNames, QFields, QCharset, QArrayDim: TUIBStatement;
      OIDs: TOIDUDFs; DefaultCharset: TCharacterSet);
    function GetFields(const Index: Integer): TMetaUDFField;
    function GetFieldsCount: Integer;
  public
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    constructor Create(AOwner: TMetaNode; ClassIndex: Integer); override;
    procedure SaveToStream(Stream: TStream); override;
    property Module: string read FModule;
    property Entry: string read FEntry;
    property Return: Smallint read FReturn;
    property Fields[const Index: Integer]: TMetaUDFField read GetFields;
    property FieldsCount: Integer read GetFieldsCount;
  end;

  TMetaRole = class(TMetaNode)
  private
    FOwner: string;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromQuery(QName, QGrants: TUIBStatement; OIDs: TOIDRoles);
    procedure LoadGrantsFromQuery(QGrants: TUIBStatement);
    function GetRoleGrants(const Index: Integer): TMetaRoleGrant;
    function GetRoleGrantsCount: Integer;
  protected
    function FindGrant(Option: Boolean): TMetaRoleGrant;
  public
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    constructor Create(AOwner: TMetaNode; ClassIndex: Integer); override;
    procedure SaveToStream(Stream: TStream); override;
    property Owner: string read FOwner;

    property Grants[const Index: Integer]: TMetaRoleGrant read GetRoleGrants;
    property GrantsCount: Integer read GetRoleGrantsCount;
  end;

  TMetaDataBase = class(TMetaNode)
  private
    FOIDDatabases: TOIDDatabases;
    FOIDTables: TOIDTables;
    FOIDViews: TOIDViews;
    FOIDProcedures: TOIDProcedures;
    FOIDUDFs: TOIDUDFs;
    FOIDRoles: TOIDRoles;
    FSysInfos: Boolean;
    FDefaultCharset: TCharacterSet;

    FSortedTables: TList;
    FSortedViews: TList;
    FIdentifiers: TAvlTree;

    procedure SortTablesByForeignKeys;
    procedure SortViewsByDependencies;

    function GetGenerators(const Index: Integer): TMetaGenerator;
    function GetGeneratorsCount: Integer;

    function FindTableIndex(const TableName: string): Integer;
    function FindDomainIndex(const DomainName: string): Integer;

    function GetTables(const Index: Integer): TMetaTable;
    function GetTablesCount: Integer;
    function GetViews(const Index: Integer): TMetaView;
    function GetViewsCount: Integer;
    function GetDomains(const Index: Integer): TMetaDomain;
    function GetDomainsCount: Integer;

    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadDependencies(QDeps: TUIBStatement);

    function GetProcedures(const Index: Integer): TMetaProcedure;
    function GetProceduresCount: Integer;
    function GetExceptions(const Index: Integer): TMetaException;
    function GetExceptionsCount: Integer;
    function GetUDFS(const Index: Integer): TMetaUDF;
    function GetUDFSCount: Integer;
    function GetRoles(const Index: Integer): TMetaRole;
    function GetRolesCount: Integer;
    function GetSortedTables(const Index: Integer): TMetaTable;
    function GetSortedTablesCount: Integer;
    function GetSortedViews(const Index: Integer): TMetaView;
    function GetSortedViewsCount: Integer;
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToStream(Stream: TStream); override;
    function FindTableName(const TableName: string): TMetaTable;
    function FindProcName(const ProcName: string): TMetaProcedure;
    function FindExceptionName(const ExcepName: string): TMetaException;
    function FindGeneratorName(const GenName: string): TMetaGenerator;
    function FindUDFName(const UDFName: string): TMetaUDF;
    function FindViewName(const ViewName: string): TMetaView;
    function FindRoleName(const RoleName: string): TMetaRole;
    function FindDomainName(const DomainName: string): TMetaDomain;
    function FindTriggerName(const TriggerName: string): TMetaTrigger;

    constructor Create(AOwner: TMetaNode; ClassIndex: Integer); override;
    destructor Destroy; override;

    procedure LoadFromDatabase(Transaction: TUIBTransaction);
    procedure SaveToDDL(Stream: TStringStream; options: TDDLOptions); override;
    property OIDDatabases: TOIDDatabases read FOIDDatabases write FOIDDatabases;

    property Generators[const Index: Integer]: TMetaGenerator read GetGenerators;
    property GeneratorsCount: Integer read GetGeneratorsCount;

    property Tables[const Index: Integer]: TMetaTable read GetTables;
    property TablesCount: Integer read GetTablesCount;
    property OIDTables: TOIDTables read FOIDTables write FOIDTables;

    property SortedTables[const Index: Integer]: TMetaTable read GetSortedTables;
    property SortedTablesCount: Integer read GetSortedTablesCount;

    property Views[const Index: Integer]: TMetaView read GetViews;
    property ViewsCount: Integer read GetViewsCount;
    property OIDViews: TOIDViews read FOIDViews write FOIDViews;

    property SortedViews[const Index: Integer]: TMetaView read GetSortedViews;
    property SortedViewsCount: Integer read GetSortedViewsCount;

    property Domains[const Index: Integer]: TMetaDomain read GetDomains;
    property DomainsCount: Integer read GetDomainsCount;

    property Procedures[const Index: Integer]: TMetaProcedure read GetProcedures;
    property ProceduresCount: Integer read GetProceduresCount;
    property OIDProcedures: TOIDProcedures read FOIDProcedures write FOIDProcedures;

    property Exceptions[const Index: Integer]: TMetaException read GetExceptions;
    property ExceptionsCount: Integer read GetExceptionsCount;

    property UDFS[const Index: Integer]: TMetaUDF read GetUDFS;
    property UDFSCount: Integer read GetUDFSCount;
    property OIDUDFs: TOIDUDFs read FOIDUDFs write FOIDUDFs;

    property Roles[const Index: Integer]: TMetaRole read GetRoles;
    property RolesCount: Integer read GetRolesCount;
    property OIDRoles: TOIDRoles read FOIDRoles write FOIDRoles;

    property SysInfos: Boolean read FSysInfos write FSysInfos;
    property DefaultCharset: TCharacterSet read FDefaultCharset;
  end;

  { Used to store temporary grantees }
  TGrantee = record
    User: string;
    UserType: Integer;
    Grantor: string;
    Option: Boolean;
  end;

  { Grantees : Who or Which object has privileges }
  TMetaGrantee = class(TMetaNode)
  private
    FGrantor: String;
    procedure LoadFromGrantee(G: TGrantee);
  protected
    procedure LoadFromStream(Stream: TStream); override;    
  public
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    property Grantor: String read FGrantor;
  end;

  TMetaRoleGrantee = class(TMetaGrantee)
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
  end;

  TMetaUserGrantee = class(TMetaGrantee)
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
  end;

  TMetaProcedureGrantee = class(TMetaGrantee)
  public
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
  end;

  TMetaTriggerGrantee = class(TMetaGrantee)
  public
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
  end;

  TMetaViewGrantee = class(TMetaGrantee)
  public
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
  end;

  { Grants : Which privileges have been given to this object to whom }
  TMetaGrant = class(TMetaNode)
  private
    FOption: Boolean;
  protected
    procedure LoadFromStream(Stream: TStream); override;
  public
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    procedure SaveGranteesToDDLNode(Stream: TStringStream; OptionKeyWord: String; options: TDDLOptions);
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    property Option: Boolean read FOption;
  end;

  TMetaRoleGrant = class(TMetaGrant)
  private
    procedure LoadFromGrantee(G: TGrantee);
  protected
    function GetName: String; override;
  public
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    constructor Create(AOwner: TMetaNode; ClassIndex: Integer); override;
  end;

  TMetaObjectGrant = class(TMetaGrant)
  private
    procedure LoadFromGrantee(G: TGrantee);
  public
    constructor Create(AOwner: TMetaNode; ClassIndex: Integer); override;
  end;

  TMetaProcedureGrant = class(TMetaObjectGrant)
  protected
    function GetName: String; override;
  public
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
  end;

  TMetaTableGrant = class(TMetaObjectGrant)
  private
    FPrivileges : TTablePrivileges;
  protected
    function GetName: String; override;
    procedure LoadFromStream(Stream: TStream); override;
  public
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    property Privileges: TTablePrivileges read FPrivileges;
  end;

  TMetaFieldGrant = class(TMetaObjectGrant)
  private
    FPrivilege : TFieldPrivilege;
    FFields: TStringList;
    function GetFields(const Index: Integer): String;
    function GetFieldsCount: Integer;
  protected
    function GetName: String; override;
    procedure LoadFromStream(Stream: TStream); override;    
  public
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToDDLNode(Stream: TStringStream; options: TDDLOptions); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    constructor Create(AOwner: TMetaNode; ClassIndex: Integer); override;
    destructor Destroy; override;
    function HasFields(F: TStringList): Boolean;
    procedure AssignFields(F: TStringList);
    property Privilege: TFieldPrivilege read FPrivilege;
    property Fields[const Index: Integer]: String read GetFields;
    property FieldsCount: Integer read GetFieldsCount;
  end;

implementation

{$IFDEF UNICODE}
uses
  AnsiStrings;
{$ENDIF}

//   Database Tree
//------------------------
//  OIDDomains   = 0;
//  OIDTable     = 1;
//    OIDTableFields   = 0;
//    OIDPrimary       = 1;
//    OIDForeign       = 2;
//    OIDTableTrigger  = 3;
//    OIDUnique        = 4;
//    OIDIndex         = 5;
//    OIDCheck         = 6;
//  OIDView      = 2;
//    OIDViewFields    = 0;
//    OIDViewTrigers   = 1;
//  OIDProcedure = 3;
//    OIDProcFieldIn   = 0;
//    OIDProcFieldOut  = 1;
//  OIDGenerator = 4;
//  OIDException = 5;
//  OIDUDF       = 6;
//    OIDUDFField      = 0;
//  OIDRole      = 7;
//    OIDRoleGrantee   = 0;

const

  TriggerPrefixTypes: array [TTriggerPrefix] of string =
    ('BEFORE', 'AFTER');

  TriggerSuffixTypes: array [TTriggerSuffix] of string =
    ('INSERT', 'UPDATE', 'DELETE');

  FieldTypes: array [TUIBFieldType] of string =
   ('', 'NUMERIC', 'CHAR', 'VARCHAR', 'CSTRING', 'SMALLINT', 'INTEGER', 'QUAD',
    'FLOAT', 'DOUBLE PRECISION', 'TIMESTAMP', 'BLOB', 'BLOBID', 'DATE', 'TIME',
    'BIGINT' , 'ARRAY'{$IFDEF IB7_UP}, 'BOOLEAN' {$ENDIF}
    {$IFDEF FB25_UP}, 'NULL'{$ENDIF});

  QRYDefaultCharset =
    'select ' +
    '  RDB$CHARACTER_SET_NAME ' +
    'from ' +
    '  RDB$DATABASE';

  QRYGenerators =
    'select ' +
    '  RDB$GENERATOR_NAME ' +
    'from ' +
    '  RDB$GENERATORS GEN ' +
    'where ' +
    '  (not GEN.RDB$GENERATOR_NAME starting with ''RDB$'') and ' +
    '  (not GEN.RDB$GENERATOR_NAME starting with ''SQL$'') and ' +
    '  ((GEN.RDB$SYSTEM_FLAG is null) or (GEN.RDB$SYSTEM_FLAG <> 1)) ' +
    'order by ' +
    '  GEN.RDB$GENERATOR_NAME';

  QRYTables =
    'select ' +
    '  REL.RDB$RELATION_NAME ' +
    'from ' +
    '  RDB$RELATIONS REL ' +
    'where ' +
    '  (REL.RDB$SYSTEM_FLAG <> 1 or REL.RDB$SYSTEM_FLAG is null) and ' +
    '  (NOT REL.RDB$FLAGS is null) and ' +
    '  (REL.RDB$VIEW_BLR is null) and ' +
    '  (REL.RDB$SECURITY_CLASS starting with ''SQL$'') ' +
    'order by ' +
    '  REL.RDB$RELATION_NAME';

  QRYSysTables =
    'select ' +
    '  REL.RDB$RELATION_NAME ' +
    'from ' +
    '  RDB$RELATIONS REL ' +
    'where ' +
    '  REL.RDB$VIEW_BLR IS NULL ' +
    'order by ' +
    '  REL.RDB$RELATION_NAME';

  QRYTableFields =
    'select ' +
    '  FLD.RDB$FIELD_TYPE' +
    ', FLD.RDB$FIELD_SCALE' +
    ', FLD.RDB$FIELD_LENGTH' +
    ', FLD.RDB$FIELD_PRECISION' +
    ', FLD.RDB$CHARACTER_SET_ID' +   // CHARACTER SET
    ', RFR.RDB$COLLATION_ID' +
    ', COL.RDB$COLLATION_NAME' +     // COLLATE
    ', FLD.RDB$FIELD_SUB_TYPE' +
    ', RFR.RDB$DEFAULT_SOURCE' +     // DEFAULT
    ', RFR.RDB$FIELD_NAME' +
    ', FLD.RDB$SEGMENT_LENGTH' +
    ', FLD.RDB$SYSTEM_FLAG'+
    ', RFR.RDB$NULL_FLAG' +          // NULLABLE
    ', FLD.RDB$VALIDATION_SOURCE' +  // CHECK
    ', FLD.RDB$DIMENSIONS'+
    ', RFR.RDB$FIELD_SOURCE' +
    ', FLD.RDB$COMPUTED_SOURCE' +    // COMPUTED BY
    ', RDB$VALIDATION_SOURCE ' +
    'from ' +
    '  RDB$RELATIONS REL ' +
    'join RDB$RELATION_FIELDS RFR on (RFR.RDB$RELATION_NAME = REL.RDB$RELATION_NAME) ' +
    'join RDB$FIELDS FLD on (RFR.RDB$FIELD_SOURCE = FLD.RDB$FIELD_NAME) ' +
    'left outer join RDB$COLLATIONS COL on (COL.RDB$COLLATION_ID = RFR.RDB$COLLATION_ID and COL.RDB$CHARACTER_SET_ID = FLD.RDB$CHARACTER_SET_ID) ' +
    'where ' +
    '  (REL.RDB$RELATION_NAME = ?) ' +
    'order by ' +
    '  RFR.RDB$FIELD_POSITION, RFR.RDB$FIELD_NAME';

  QRYCharset =
    'select ' +
    '  RDB$CHARACTER_SET_ID' +
    ', RDB$CHARACTER_SET_NAME' +
    ', RDB$BYTES_PER_CHARACTER ' +
    'from ' +
    '  RDB$CHARACTER_SETS';

  QRYUnique =
    'select ' +
    '  RC.RDB$CONSTRAINT_NAME' +
    ', IDX.RDB$FIELD_NAME' +
    ', RC.RDB$INDEX_NAME as PK_INDEX' +
    ', I.RDB$INDEX_TYPE as PK_INDEX_TYPE ' +
    'from ' +
    '  RDB$RELATION_CONSTRAINTS RC ' +
    'join RDB$INDEX_SEGMENTS IDX on (IDX.RDB$INDEX_NAME = RC.RDB$INDEX_NAME) ' +
    'join RDB$INDICES I on (I.RDB$INDEX_NAME = RC.RDB$INDEX_NAME) ' +
    'where ' +
    '  (RC.RDB$CONSTRAINT_TYPE = ?) and (RC.RDB$RELATION_NAME = ?) ' +
    'order by ' +
    '  RC.RDB$RELATION_NAME, IDX.RDB$FIELD_POSITION';

  QRYIndex =
    'select ' +
    '  IDX.RDB$INDEX_NAME' +
    ', ISG.RDB$FIELD_NAME' +
    ', IDX.RDB$UNIQUE_FLAG' +
    ', IDX.RDB$INDEX_INACTIVE' +
    ', IDX.RDB$INDEX_TYPE ' +
{$IFDEF FB21_UP}
    ', IDX.RDB$EXPRESSION_SOURCE ' +
{$ENDIF}
    'from ' +
    '  RDB$INDICES IDX ' +
    'left join RDB$INDEX_SEGMENTS ISG on (ISG.RDB$INDEX_NAME = IDX.RDB$INDEX_NAME) ' +
    'left join RDB$RELATION_CONSTRAINTS C on (IDX.RDB$INDEX_NAME = C.RDB$INDEX_NAME) ' +
    'where ' +
    '  (C.RDB$CONSTRAINT_NAME is null) and (IDX.RDB$RELATION_NAME = ?) ' +
    'order by ' +
    '  IDX.RDB$RELATION_NAME, IDX.RDB$INDEX_NAME, ISG.RDB$FIELD_POSITION';

  QRYForeign =
    'select ' +
    '  A.RDB$CONSTRAINT_NAME' +
    ', B.RDB$UPDATE_RULE' +
    ', B.RDB$DELETE_RULE' +
    ', C.RDB$RELATION_NAME AS FK_TABLE' +
    ', D.RDB$FIELD_NAME AS FK_FIELD' +
    ', E.RDB$FIELD_NAME AS ONFIELD' +
    ', A.RDB$INDEX_NAME AS FK_INDEX' +
    ', I.RDB$INDEX_TYPE AS FK_INDEX_TYPE ' +
    'from ' +
    '  RDB$REF_CONSTRAINTS B ' +
    'join RDB$RELATION_CONSTRAINTS A on (A.RDB$CONSTRAINT_NAME=B.RDB$CONSTRAINT_NAME) ' +
    'join RDB$RELATION_CONSTRAINTS C on (B.RDB$CONST_NAME_UQ=C.RDB$CONSTRAINT_NAME) ' +
    'join RDB$INDEX_SEGMENTS D on (C.RDB$INDEX_NAME=D.RDB$INDEX_NAME) ' +
    'join RDB$INDEX_SEGMENTS E on (A.RDB$INDEX_NAME=E.RDB$INDEX_NAME and D.RDB$FIELD_POSITION=E.RDB$FIELD_POSITION) ' +
    'join RDB$INDICES I ON (I.RDB$INDEX_NAME = A.RDB$INDEX_NAME) ' +
    'where ' +
    '  (A.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'') and (A.RDB$RELATION_NAME = ?) ' +
    'order by ' +
    '  A.RDB$CONSTRAINT_NAME, A.RDB$RELATION_NAME, D.RDB$FIELD_POSITION, E.RDB$FIELD_POSITION';

  QRYCheck =
    'select ' +
    '  A.RDB$CONSTRAINT_NAME' +
    ', C.RDB$TRIGGER_SOURCE ' +
    'from ' +
    '  RDB$RELATION_CONSTRAINTS A ' +
    'join RDB$CHECK_CONSTRAINTS B on (A.RDB$CONSTRAINT_NAME = B.RDB$CONSTRAINT_NAME) ' +
    'join RDB$TRIGGERS C on (B.RDB$TRIGGER_NAME = C.RDB$TRIGGER_NAME)' +
    'where ' +
    '  (A.RDB$CONSTRAINT_TYPE = ''CHECK'') and (C.RDB$TRIGGER_TYPE = 1) and ' +
    '  (A.RDB$RELATION_NAME = ?)';

  QRYTrigger =
    'select ' +
    '  T.RDB$TRIGGER_NAME' +
    ', T.RDB$TRIGGER_SOURCE' +
    ', T.RDB$TRIGGER_SEQUENCE' +
    ', T.RDB$TRIGGER_TYPE' +
    ', T.RDB$TRIGGER_INACTIVE' +
    ', T.RDB$SYSTEM_FLAG ' +
    'from ' +
    '  RDB$TRIGGERS T ' +
    'left join RDB$CHECK_CONSTRAINTS C ON (C.RDB$TRIGGER_NAME = T.RDB$TRIGGER_NAME) ' +
    'where ' +
    '  ((T.RDB$SYSTEM_FLAG = 0) or (T.RDB$SYSTEM_FLAG is null)) and ' +
    '  (C.RDB$TRIGGER_NAME is null) and (T.RDB$RELATION_NAME = ?) ' +
    'order by ' +
    '  T.RDB$TRIGGER_NAME';

  QRYSysTrigger =
    'select ' +
    '  T.RDB$TRIGGER_NAME' +
    ', T.RDB$TRIGGER_SOURCE' +
    ', T.RDB$TRIGGER_SEQUENCE' +
    ', T.RDB$TRIGGER_TYPE' +
    ', T.RDB$TRIGGER_INACTIVE' +
    ', T.RDB$SYSTEM_FLAG ' +
    'from ' +
    '  RDB$TRIGGERS T ' +
    'left join RDB$CHECK_CONSTRAINTS C on (C.RDB$TRIGGER_NAME = T.RDB$TRIGGER_NAME) ' +
    'where ' +
    '  (T.RDB$RELATION_NAME = ?) ' +
    'order by ' +
    '  T.RDB$TRIGGER_NAME';

  QRYView =
    'select ' +
    '  REL.RDB$RELATION_NAME' +
    ', REL.RDB$VIEW_SOURCE ' +
    'from ' +
    '  RDB$RELATIONS REL ' +
    'where ' +
    '  (REL.RDB$SYSTEM_FLAG <> 1 or REL.RDB$SYSTEM_FLAG is null) and ' +
    '  (not REL.RDB$FLAGS is null) and ' +
    '  (not REL.RDB$VIEW_BLR is null) and ' +
    '  (REL.RDB$SECURITY_CLASS starting with ''SQL$'') ' +
    'order by ' +
    '  REL.RDB$RELATION_NAME';

  QRYDomains =
    'select ' +
    '  FLD.RDB$FIELD_TYPE' +
    ', FLD.RDB$FIELD_SCALE' +
    ', FLD.RDB$FIELD_LENGTH' +
    ', FLD.RDB$FIELD_PRECISION' +
    ', FLD.RDB$CHARACTER_SET_ID' +  // CHARACTER SET
    ', FLD.RDB$COLLATION_ID' +
    ', COL.RDB$COLLATION_NAME' +    // COLLATE
    ', FLD.RDB$FIELD_SUB_TYPE' +
    ', FLD.RDB$DEFAULT_SOURCE' +    // DEFAULT
    ', FLD.RDB$FIELD_NAME' +
    ', FLD.RDB$SEGMENT_LENGTH' +
    ', FLD.RDB$SYSTEM_FLAG' +
    ', FLD.RDB$NULL_FLAG' +         // NULLABLE
    ', FLD.RDB$VALIDATION_SOURCE' + // CHECK
    ', FLD.RDB$DIMENSIONS ' +
    'from ' +
    '  RDB$FIELDS FLD ' +
    'left outer join RDB$COLLATIONS COL on (COL.RDB$CHARACTER_SET_ID = FLD.RDB$CHARACTER_SET_ID and COL.RDB$COLLATION_ID = FLD.RDB$COLLATION_ID) ' +
    'where ' +
    '  not (FLD.RDB$FIELD_NAME starting with ''RDB$'')';

  QRYSysDomains =
    'select ' +
    '  RDB$FIELD_TYPE' +
    ', RDB$FIELD_SCALE' +
    ', RDB$FIELD_LENGTH' +
    ', RDB$FIELD_PRECISION' +
    ', FLD.RDB$CHARACTER_SET_ID' +      // CHARACTER SET
    ', FLD.RDB$COLLATION_ID' +
    ', COL.RDB$COLLATION_NAME' +    // COLLATE
    ', RDB$FIELD_SUB_TYPE' +
    ', RDB$DEFAULT_SOURCE' +        // DEFAULT
    ', RDB$FIELD_NAME' +
    ', RDB$SEGMENT_LENGTH' +
    ', FLD.RDB$SYSTEM_FLAG'+
    ', RDB$NULL_FLAG' +             // NULLABLE
    ', RDB$VALIDATION_SOURCE' +     // CHECK
    ', RDB$DIMENSIONS ' +
    'from ' +
    '  RDB$FIELDS FLD ' +
    'left outer join RDB$COLLATIONS COL on (COL.RDB$CHARACTER_SET_ID = FLD.RDB$CHARACTER_SET_ID and COL.RDB$COLLATION_ID = FLD.RDB$COLLATION_ID) ';

  QRYProcedures =
    'select ' +
    '  RDB$PROCEDURE_NAME' +
    ', RDB$PROCEDURE_SOURCE ' +
    'from ' +
    '  RDB$PROCEDURES ' +
    'order by ' +
    '  RDB$PROCEDURE_NAME';

  QRYProcFields =
    'select ' +
    '  FLD.RDB$FIELD_TYPE ' +
    ', FLD.RDB$FIELD_SCALE ' +
    ', FLD.RDB$FIELD_LENGTH ' +
    ', FLD.RDB$FIELD_PRECISION ' +
    ', FLD.RDB$CHARACTER_SET_ID ' +  // CHARACTER SET
    ', FLD.RDB$COLLATION_ID ' +
    ', COL.RDB$COLLATION_NAME ' +    // COLLATE
    ', FLD.RDB$FIELD_SUB_TYPE ' +
    ', FLD.RDB$DEFAULT_SOURCE ' +    // DEFAULT
    ', PPA.RDB$PARAMETER_NAME ' +
    ', FLD.RDB$SEGMENT_LENGTH  ' +
    ', FLD.RDB$SYSTEM_FLAG ' +
    'from  ' +
    '  RDB$PROCEDURES PRO ' +
    'join RDB$PROCEDURE_PARAMETERS PPA on (PPA.RDB$PROCEDURE_NAME = PRO.RDB$PROCEDURE_NAME) ' +
    'join RDB$FIELDS FLD on (FLD.RDB$FIELD_NAME = PPA.RDB$FIELD_SOURCE) ' +
    'left outer join RDB$COLLATIONS COL on (FLD.RDB$COLLATION_ID = COL.RDB$COLLATION_ID and FLD.RDB$CHARACTER_SET_ID = COL.RDB$CHARACTER_SET_ID) ' +
    'where ' +
    '  (PRO.RDB$PROCEDURE_NAME = ?) and (PPA.RDB$PARAMETER_TYPE = ?) ' +
    'order by ' +
    '  PRO.RDB$PROCEDURE_NAME, PPA.RDB$PARAMETER_TYPE, PPA.RDB$PARAMETER_NUMBER';

  QRYExceptions =
    'select ' +
    '  RDB$EXCEPTION_NAME' +
    ', RDB$MESSAGE' +
    ', RDB$EXCEPTION_NUMBER ' +
    'from ' +
    '  RDB$EXCEPTIONS ' +
    'order by ' +
    '  RDB$EXCEPTION_NAME';

  QRYUDF =
    'select ' +
    '  RDB$FUNCTION_NAME' +
    ', RDB$MODULE_NAME' +
    ', RDB$ENTRYPOINT' +
    ', RDB$RETURN_ARGUMENT ' +
    'from ' +
    '  RDB$FUNCTIONS ' +
    'where ' +
    '  (RDB$SYSTEM_FLAG is null) or (RDB$SYSTEM_FLAG = 0) ' +
    'order by ' +
    '  RDB$FUNCTION_NAME';

  QRYUDFFields =
    'select ' +
    '  RDB$FIELD_TYPE' +
    ', RDB$FIELD_SCALE' +
    ', RDB$FIELD_LENGTH' +
    ', RDB$FIELD_PRECISION' +
    ', RDB$CHARACTER_SET_ID' +
    ', NULL' +
    ', NULL' +
    ', RDB$FIELD_SUB_TYPE' +
    ', NULL' +
    ', RDB$ARGUMENT_POSITION' +
    ', RDB$MECHANISM ' +
    ', NULL '+
    'from ' +
    '  RDB$FUNCTION_ARGUMENTS ' +
    'where ' +
    '  RDB$FUNCTION_NAME = ? ' +
    'order by ' +
    '  RDB$ARGUMENT_POSITION';

  QRYRoles =
    'select ' +
    '  RDB$ROLE_NAME' +
    ', RDB$OWNER_NAME ' +
    'from ' +
    '  RDB$ROLES ' +
    'where ' +
    '  not RDB$ROLE_NAME starting with ''RDB$''';

  QRYArrayDim =
    'select ' +
    '  RDB$LOWER_BOUND' +
    ', RDB$UPPER_BOUND ' +
    'from ' +
    '  RDB$FIELD_DIMENSIONS DIM '+
    'where ' +
    '  DIM.RDB$FIELD_NAME = ? ' +
    'order by ' +
    '  DIM.RDB$DIMENSION';

  {
    Some grants can be ignored, sometimes, by adding
    "RDB$GRANT_OPTION is not NULL" to where clause...
    Perhaps GRANTS could be filtered using RDB$USER_NAME <> RDB$GRANTOR since
    there is no reason for a GRANTOR to GRANTS rights to himself...
  }

  QRYRelationGrants =
    'select ' +
    '  RDB$USER' +
    ', RDB$GRANTOR' +
    ', RDB$PRIVILEGE' +
    ', RDB$GRANT_OPTION' +
    ', RDB$USER_TYPE ' +
    'from ' +
    '  RDB$USER_PRIVILEGES ' +
    'where ' +
    '  (RDB$USER <> RDB$GRANTOR) and (RDB$OBJECT_TYPE = ?) and ' +
    '  (RDB$RELATION_NAME = ?) and (RDB$FIELD_NAME is null) ' +
    'order by ' +
    '  RDB$GRANT_OPTION, RDB$USER_TYPE, RDB$USER, RDB$GRANTOR';

  QRYFieldGrants =
    'select ' +
    '  RDB$USER' +
    ', RDB$GRANTOR' +
    ', RDB$PRIVILEGE' +
    ', RDB$GRANT_OPTION' +
    ', RDB$USER_TYPE' +
    ', RDB$FIELD_NAME ' +
    'from ' +
    '  RDB$USER_PRIVILEGES ' +
    'where ' +
    '  (RDB$USER <> RDB$GRANTOR) and (RDB$OBJECT_TYPE = ?) and ' +
    '  (RDB$RELATION_NAME = ?) and (RDB$FIELD_NAME is not null) ' +
    'order by ' +
    '  RDB$PRIVILEGE, RDB$GRANT_OPTION, RDB$USER_TYPE, RDB$USER, RDB$GRANTOR, RDB$FIELD_NAME';

  QRYDependendies =
    'select distinct ' +
    '  d.RDB$DEPENDENT_NAME, ' +
    '  d.RDB$DEPENDENT_TYPE, ' +
    '  d.RDB$DEPENDED_ON_NAME, ' +
    '  d.RDB$DEPENDED_ON_TYPE ' +
    'from ' +
    '  RDB$DEPENDENCIES d ' +
    'join RDB$TYPES t on (d.RDB$DEPENDENT_TYPE=t.RDB$TYPE and t.RDB$FIELD_NAME = ''RDB$OBJECT_TYPE'')' +
    'join RDB$TYPES ot on (d.RDB$DEPENDED_ON_TYPE=ot.RDB$TYPE and ot.RDB$FIELD_NAME = ''RDB$OBJECT_TYPE'')';

procedure WriteString(Stream: TStream; const Str: string);
var
  Len: Integer;
begin
  Len := Length(Str);
  Stream.Write(Len, SizeOf(Len));
  if Len > 0 then
    Stream.Write(PChar(Str)^, Len * SizeOf(Char));
end;

procedure ReadString(Stream: TStream; var Str: string);
var
  Len: Integer;
begin
  Stream.Read(Len, SizeOf(Len));
  SetLength(Str, Len);
  if Len > 0 then
    Stream.Read(PChar(Str)^, Len * SizeOf(Char));
end;

{ TAVLString }
type
  TAVLString = class(TAvlHandle)
  private
    FName: string;
  public
    constructor Create(const AName: string); 
  end;

constructor TAVLString.Create(const AName: string);
begin
  FName := AName;
end;

{ TAvlStringTree }

type
  TAvlStringTree = class(TAvlTree)
  protected
    function CompareNodeNode(node1, node2: TAvlHandle): integer; override;
    function CompareKeyNode(k: TAvlKey; h: TAvlHandle): integer; override;
  end;

function TAvlStringTree.CompareKeyNode(k: TAvlKey; h: TAvlHandle): integer;
begin
  Result := CompareText(PChar(k), TAVLString(h).FName);
end;

function TAvlStringTree.CompareNodeNode(node1, node2: TAvlHandle): integer;
begin
  Result := CompareText(TAVLString(node1).FName, TAVLString(node2).FName);
end;

{ TMetaNode }

constructor TMetaNode.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  inherited Create;
  FData := nil;
  FNodeItemsCount := 0;
  FOwner := AOwner;
  if (FOwner <> nil) and (ClassIndex >= 0) then
    FOwner.FNodeItems[ClassIndex].Childs.Add(Self)
end;

constructor TMetaNode.CreateFromStream(AOwner: TMetaNode; ClassIndex: Integer; Stream: TStream);
var
  I, J: Integer;
begin
  Create(AOwner, ClassIndex);
  LoadFromStream(Stream);
  for J := 0 to FNodeItemsCount - 1 do
  begin
    Stream.Read(I, SizeOf(I));
    for I := 0 to I - 1 do
      FNodeItems[J].ClassID.CreateFromStream(Self, J, Stream);
  end;
end;

destructor TMetaNode.Destroy;
var
  I, J: Integer;
begin
  SetLength(FDependents, 0);
  SetLength(FDependedOn, 0);
  for I := 0 to FNodeItemsCount - 1 do
  begin
    for J := 0 to FNodeItems[I].Childs.Count - 1 do
      TObJect(FNodeItems[I].Childs[J]).Free;
    FNodeItems[I].Childs.Free;
  end;
  inherited Destroy;
end;

function TMetaNode.GetAsDDL: string;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    SaveToDDL(Stream, []);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

function TMetaNode.GetItems(const ClassIndex, Index: Integer): TMetaNode;
var
  FChilds: TList;
begin
  FChilds := FNodeItems[ClassIndex].Childs;
  if (FChilds.Count > 0) and (Index >= 0) and
    (Index < FChilds.Count) then
    Result := TMetaNode(FChilds.Items[Index])
  else
    raise EUIBError.CreateFmt(EUIB_INDEXERROR, [Index]);
end;

procedure TMetaNode.SaveToStream(Stream: TStream);
var
  I, J: Integer;
begin
  for J := 0 to FNodeItemsCount - 1 do
  begin
    I := FNodeItems[J].Childs.Count;
    Stream.Write(I, SizeOf(I));
    for I := 0 to I - 1 do
      TMetaNode(FNodeItems[J].Childs.Items[I]).SaveToStream(Stream);
  end;
end;

procedure TMetaNode.AddClass(ClassID: TMetaNodeClass);
begin
  SetLength(FNodeItems, FNodeItemsCount + 1);
  FNodeItems[FNodeItemsCount].Childs := TList.Create;
  FNodeItems[FNodeItemsCount].ClassID := ClassID;
  Inc(FNodeItemsCount);
end;

procedure TMetaNode.CheckTransaction(Transaction: TUIBTransaction);
begin
  Assert(Transaction <> nil);
  Assert(Transaction.DataBase <> nil);
end;

procedure TMetaNode.SaveNode(Stream: TStringStream; OID: Integer;
  options: TDDLOptions; Separator: string);
var
  I: Integer;
begin
  for I := 0 to FNodeItems[OID].Childs.Count - 1 do
  begin
    if I = 0 then
      Stream.WriteString(NewLine)
    else
      Stream.WriteString(Separator);
    TMetaNode(FNodeItems[OID].Childs[I]).SaveToDDL(Stream, options);
  end;
end;

procedure TMetaNode.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
begin
end;

function TMetaNode.GetName: String;
begin
  Result := FName;
end;

function TMetaNode.GetNodes(const Index: Integer): TNodeItem;
begin
  Assert((Index >= 0) and (FNodeItemsCount > 0) and (Index < FNodeItemsCount));
  Result := FNodeItems[Index];
end;

function TMetaNode.MetaQuote(const str: string): string;
begin
  if (GetDatabase.FIdentifiers.Search(PChar(str)) <> nil) then
    Result := '"' + str + '"' else
    Result := SQLQuote(str);
end;

class function TMetaNode.NodeClass: string;
begin
  Result := 'Node'
end;

procedure TMetaNode.SaveToDDL(Stream: TStringStream; options: TDDLOptions);
begin
  SaveToDDLNode(Stream, options);
end;

function TMetaNode.GetAsDDLNode: string;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    SaveToDDLNode(Stream, []);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

function TMetaNode.GetAsFullDDL: string;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    SaveToDDL(Stream, [ddlFull]);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

function TMetaNode.GetAsFullDDLNode: string;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    SaveToDDLNode(Stream, [ddlFull]);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

function TMetaNode.GetDatabase: TMetaDatabase;
begin
  case NodeType of
    MetaDatabase: Result := TMetaDatabase(Self);
    MetaDomain, MetaTable, MetaView, MetaProcedure, MetaGenerator,
      MetaException, MetaUDF, MetaRole: Result := TMetaDatabase(FOwner);
  else
    Result := TMetaDatabase(FOwner.FOwner);
  end;
end;

function TMetaNode.GetDependedOn(const Index: Integer): TMetaNode;
begin
  Result := FDependedOn[Index];
end;

function TMetaNode.GetDependedOnCount: Integer;
begin
  Result := Length(FDependedOn);
end;

function TMetaNode.GetDependent(const Index: Integer): TMetaNode;
begin
  Result := FDependents[Index];
end;

function TMetaNode.GetDependentCount: Integer;
begin
  Result := Length(FDependents);
end;

class function TMetaNode.NodeType: TMetaNodeType;
begin
  Result := MetaNode;
end;

procedure TMetaNode.RegisterDependedOn(OtherNode: TMetaNode);
begin
  SetLength(FDependedOn, Length(FDependedOn) + 1);
  FDependedOn[Length(FDependedOn) -1] := OtherNode;
end;

procedure TMetaNode.RegisterDependent(OtherNode: TMetaNode);
begin
  SetLength(FDependents, Length(FDependents) + 1);
  FDependents[Length(FDependents) -1] := OtherNode;
end;

{ TMetaGenerator }

procedure TMetaGenerator.LoadFromDataBase(Transaction: TUIBTransaction;
  const Name: string);
var
  Query: TUIBStatement;
begin
  CheckTransaction(Transaction);
  Query := TUIBStatement.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  Query.Transaction := Transaction;
  Query.CachedFetch := False;
  try
    FName := MetaQuote(Name);
    Query.SQL.Text := Format('select gen_id(%s, 0) from rdb$database', [Self.Name]);
    Query.Open;
    if not Query.Eof then
      FValue := Query.Fields.AsInteger[0]
    else
      raise EUIBError.CreateFmt(EUIB_NOGENERATOR, [FName]);
  finally
    Query.Free;
  end;
end;

procedure TMetaGenerator.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  Stream.Read(FValue, SizeOf(FValue));
end;

procedure TMetaGenerator.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  Stream.Write(FValue, SizeOf(FValue));
end;

procedure TMetaGenerator.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
begin
  SaveToCreateDDLNode(Stream);
  Stream.WriteString(NewLine);
  SaveToAlterDDLNode(Stream);
end;

class function TMetaGenerator.NodeClass: string;
begin
  Result := 'Generator';
end;

class function TMetaGenerator.NodeType: TMetaNodeType;
begin
  Result := MetaGenerator;
end;

procedure TMetaGenerator.SaveToAlterDDLNode(Stream: TStringStream);
begin
  Stream.WriteString(Format('SET GENERATOR %s TO %d;', [Name, FValue]));
end;

procedure TMetaGenerator.SaveToCreateDDLNode(Stream: TStringStream);
begin
  Stream.WriteString(Format('CREATE GENERATOR %s;', [Name]));
end;

function TMetaGenerator.GetAsAlterDDL: string;
var stream: TStringStream;
begin
  stream := TStringStream.Create('');
  try
    SaveToAlterDDLNode(stream);
    Result := stream.DataString;
  finally
    stream.Free;
  end;
end;

function TMetaGenerator.GetAsCreateDLL: string;
var stream: TStringStream;
begin
  stream := TStringStream.Create('');
  try
    SaveToCreateDDLNode(stream);
    Result := stream.DataString;
  finally
    stream.Free;
  end;
end;

{ TMetaBaseTable }

function TMetaBaseTable.FindFieldGrant(const Privilege: TFieldPrivilege;
  Option: Boolean; Fields: TStringList): TMetaFieldGrant;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FieldsGrantsCount - 1 do
    if (FieldsGrants[I].Privilege = Privilege) and (FieldsGrants[I].Option = Option) and
       (FieldsGrants[I].HasFields(Fields)) then
    begin
      Result := FieldsGrants[I];
      Exit;
    end;
end;

function TMetaBaseTable.FindGrant(const Privileges: TTablePrivileges;
  Option: Boolean): TMetaTableGrant;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to GrantsCount - 1 do
    if (Grants[I].Privileges = Privileges) and (Grants[I].Option = Option) then
    begin
      Result := Grants[I];
      Exit;
    end;
end;

procedure TMetaBaseTable.LoadFieldsGrantsFromQuery(G: TUIBStatement);
var
  Current: TGrantee;
  Previous: TGrantee;
  CurrentPrivilege, PreviousPrivilege: TFieldPrivilege;
  Grant: TMetaFieldGrant;
  Privilege: AnsiChar;
  Fields: TStringList;
  FieldName: string;

  function DecodePrivilege: TFieldPrivilege;
  begin
    case Privilege of
    'U' : Result := fpUpdate;
    'R' : Result := fpReference;
    else
      raise EUIBError.CreateFmt('Unsupported FIELD GRANT PRIVILEGE %s',[Privilege]);
    end;
  end;

begin
  Fields := TStringList.Create;
  try
    Current.UserType := -1;
    Previous.UserType := -1;

    { Fake initialisation, to make delphi compiler happy }
    PreviousPrivilege := TFieldPrivilege(-1);
    CurrentPrivilege := TFieldPrivilege(-1);

    while not G.Eof do
    begin
      if G.Fields.IsNull[2] then
        Privilege := #0
      else
        Privilege := G.Fields.AsAnsiString[2][1];

      FieldName := Trim(G.Fields.AsString[5]);

      Current.User := Trim(G.Fields.AsString[0]);
      Current.Grantor := Trim(G.Fields.AsString[1]);
      Current.Option := G.Fields.AsInteger[3] = 1;
      Current.UserType := G.Fields.AsInteger[4];
      CurrentPrivilege := DecodePrivilege;

      // Au premier tour, on se contente d'enregistrer ce qu'on a lu
      if Previous.UserType = -1 then
      begin
        Previous := Current;
        PreviousPrivilege := CurrentPrivilege;
        Fields.Add(string(FieldName));
      end
      else
      begin
        if (CurrentPrivilege = PreviousPrivilege) and
           (Current.Option = Previous.Option) and
           (Current.User = Previous.User) and
           (Current.UserType = Previous.UserType) and
           (Current.Grantor = Previous.Grantor) then
        begin
          // Même privilège (avec option), même utilisateur
          Fields.Add(string(FieldName));
        end
        else
        begin
          // Utilisateur ou privilège différent
          Grant := FindFieldGrant(PreviousPrivilege, Previous.Option, Fields);
          if not Assigned(Grant) then
          begin
            Grant := TMetaFieldGrant.Create(Self,FFieldsGrantsClassIndex);
            Grant.FPrivilege := PreviousPrivilege;
            Grant.AssignFields(Fields);
          end;
          Grant.LoadFromGrantee(Previous);

          Previous := Current;
          PreviousPrivilege := CurrentPrivilege;
          Fields.Clear;
          Fields.Add(string(FieldName));
        end;
      end;

      G.Next;
    end;

    if Current.UserType <> -1 then
    begin
      Grant := FindFieldGrant(CurrentPrivilege, Current.Option, Fields);
      if not Assigned(Grant) then
      begin
        Grant := TMetaFieldGrant.Create(Self,FFieldsGrantsClassIndex);
        Grant.FPrivilege := CurrentPrivilege;
        Grant.AssignFields(Fields);
      end;

      Grant.LoadFromGrantee(Current);
    end;
  finally
    Fields.Free;
  end;
end;

procedure TMetaBaseTable.LoadGrantsFromQuery(G: TUIBStatement);
var
  Current: TGrantee;
  Previous: TGrantee;
  Privileges: TTablePrivileges;
  Grant: TMetaTableGrant;
  Privilege: AnsiChar;

  procedure IncludePrivilege;
  begin
    case Privilege of
    'S' : Include(Privileges,tpSelect);
    'I' : Include(Privileges,tpInsert);
    'U' : Include(Privileges,tpUpdate);
    'D' : Include(Privileges,tpDelete);
    'R' : Include(Privileges,tpReference);
    else
      raise EUIBError.CreateFmt('Unsupported GRANT PRIVILEGE %s',[Privilege]);
    end;
  end;

begin
  Privileges := [];
  Current.UserType := -1;
  Previous.UserType := -1;

  while not G.Eof do
  begin
    if G.Fields.IsNull[2] then
      Privilege := #0
    else
      Privilege := G.Fields.AsAnsiString[2][1];

    Current.User := Trim(G.Fields.AsString[0]);
    Current.Grantor := Trim(G.Fields.AsString[1]);
    Current.Option := G.Fields.AsInteger[3] = 1;
    Current.UserType := G.Fields.AsInteger[4];

    if (Previous.UserType <> -1) and (
         (Current.UserType = Previous.UserType) and
         (Current.User = Previous.User) and
         (Current.Grantor = Previous.Grantor) and
         (Current.Option = Previous.Option)
        ) then
    begin
      IncludePrivilege;
    end
    else
    begin
      if Previous.UserType <> -1 then
      begin
        Grant := FindGrant(Privileges, Previous.Option);
        if not Assigned(Grant) then
        begin
          Grant := TMetaTableGrant.Create(Self,FGrantsClassIndex);
          Grant.FPrivileges := Privileges;
        end;

        Grant.LoadFromGrantee(Previous);

        Previous := Current;
        Privileges := [];
        IncludePrivilege;
      end
      else
      begin
        Previous := Current;
        IncludePrivilege;
      end;
    end;

    G.Next;
  end;

  if Current.UserType <> -1 then
  begin
    Grant := FindGrant(Privileges, Current.Option);
    if not Assigned(Grant) then
    begin
      Grant := TMetaTableGrant.Create(Self,FGrantsClassIndex);
      Grant.FPrivileges := Privileges;
    end;

    Grant.LoadFromGrantee(Current);
  end;
end;

function TMetaBaseTable.GetFieldsGrants(const Index: Integer): TMetaFieldGrant;
begin
  Result := TMetaFieldGrant(GetItems(FFieldsGrantsClassIndex, Index));
end;

function TMetaBaseTable.GetFieldsGrantsCount: Integer;
begin
  Result := FNodeItems[FFieldsGrantsClassIndex].Childs.Count;
end;

function TMetaBaseTable.GetTableGrants(const Index: Integer): TMetaTableGrant;
begin
  Result := TMetaTableGrant(GetItems(FGrantsClassIndex, Index));
end;

function TMetaBaseTable.GetTableGrantsCount: Integer;
begin
  Result := FNodeItems[FGrantsClassIndex].Childs.Count;
end;

{ TMetaTable }

constructor TMetaTable.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  inherited Create(AOwner, ClassIndex);
  AddClass(TMetaTableField);
  AddClass(TMetaPrimary);
  AddClass(TMetaForeign);
  AddClass(TMetaTrigger);
  AddClass(TMetaUnique);
  AddClass(TMetaIndex);
  AddClass(TMetaCheck);
  AddClass(TMetaTableGrant);
  AddClass(TMetaFieldGrant);

  FGrantsClassIndex := Ord(OIDTableGrant);
  FFieldsGrantsClassIndex := Ord(OIDTableFieldGrant);
end;

function TMetaTable.FindFieldName(const Name: string): TMetaTableField;
var
  I: Integer;
begin
  for I := 0 to FieldsCount - 1 do
    if Fields[I].FName = Name then
    begin
      Result := Fields[I];
      Exit;
    end;
  raise EUIBError.CreateFmt(EUIB_NOFIELD, [Name]);
end;

function TMetaTable.GetFields(const Index: Integer): TMetaTableField;
begin
  Result := TMetaTableField(GetItems(Ord(OIDTableField), Index))
end;

function TMetaTable.GetFieldsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDTableField)].Childs.Count;
end;

function TMetaTable.GetPrimary(const Index: Integer): TMetaPrimary;
begin
  Result := TMetaPrimary(GetItems(Ord(OIDPrimary), Index))
end;

function TMetaTable.GetPrimaryCount: Integer;
begin
  Result := FNodeItems[Ord(OIDPrimary)].Childs.Count;
end;

function TMetaTable.GetUniques(const Index: Integer): TMetaUnique;
begin
  Result := TMetaUnique(GetItems(Ord(OIDUnique), Index))
end;

function TMetaTable.GetUniquesCount: Integer;
begin
  Result := FNodeItems[Ord(OIDUnique)].Childs.Count;
end;

procedure TMetaTable.LoadFromDataBase(QNames, QFields, QCharset, QPrimary,
  QIndex, QCheck, QTrigger, QArrayDim, QGrants, QFieldGrants: TUIBStatement;
      OIDs: TOIDTables; DefaultCharset: TCharacterSet);
var
  Unk: string;
begin
  // Fields
  FName := MetaQuote(Trim(QNames.Fields.AsString[0]));

  if OIDTableField in OIDs then
  begin
    QFields.Params.AsString[0] := SQLUnQuote(FName);
    QFields.Open;
    while not QFields.Eof do
    begin
      with TMetaTableField.Create(Self, Ord(OIDTableField)) do
        LoadFromQuery(QFields, QCharset, QArrayDim, DefaultCharset);
      QFields.Next;
    end;

    // PRIMARY
    if OIDPrimary in OIDs then
    begin
      QPrimary.Params.AsString[1] := SQLUnQuote(FName);
      QPrimary.Params.AsString[0] := 'PRIMARY KEY';
      QPrimary.Open;
      if not QPrimary.Eof then
        TMetaPrimary.Create(Self, Ord(OIDPrimary)).LoadFromQuery(QPrimary);
    end;

    // INDICES
    if OIDIndex in OIDs then
    begin
      Unk := '';
      QIndex.Params.AsString[0] := SQLUnQuote(FName);
    {$IFDEF FB21_UP}
      QIndex.FetchBlobs := true;
    {$ENDIF}
      QIndex.Open;
      while not QIndex.Eof do
      begin
        if Unk <> Trim(QIndex.Fields.AsString[0]) then
          with TMetaIndex.Create(Self, Ord(OIDIndex)) do
          begin
            SetLength(FFields, 1);
            FName := MetaQuote(Trim(QIndex.Fields.AsString[0]));
          {$IFDEF FB21_UP}
            FComputedSource := QIndex.Fields.AsString[5];
          {$ENDIF}
            if Trim(QIndex.Fields.AsString[1]) <> '' then
              FFields[0] := FindFieldIndex(Trim(QIndex.Fields.AsString[1]));
            FUnique := QIndex.Fields.AsSingle[2] = 1;
            FActive := QIndex.Fields.AsSingle[3] = 0;
            if QIndex.Fields.AsSingle[4] = 0 then
              FOrder := IoAscending
            else
              FOrder := IoDescending;
            Unk := SQLUnQuote(FName);
          end
        else
          with Indices[IndicesCount - 1] do
          begin
            SetLength(FFields, FieldsCount + 1);
            FFields[FieldsCount - 1] := FindFieldIndex(Trim(QIndex.Fields.AsString[1]));
            Include(Fields[FieldsCount - 1].FInfos, fiIndice);
          end;
        QIndex.Next;
      end;
    end;

    // UNIQUE
    if OIDUnique in OIDs then
    begin
      QPrimary.Params.AsString[0] := 'UNIQUE';
      if not (OIDPrimary in OIDs) then
        QPrimary.Params.AsString[1] := SQLUnquote(FName);
      QPrimary.Open;
      while not QPrimary.Eof do
      begin
        if Unk <> Trim(QPrimary.Fields.AsString[0]) then
          with TMetaUnique.Create(Self, Ord(OIDUnique)) do
          begin
            SetLength(FFields, 1);
            FName := MetaQuote(Trim(QPrimary.Fields.AsString[0]));
            FFields[0] := FindFieldIndex(Trim(QPrimary.Fields.AsString[1]));

            // FB15up
            FIndexName := MetaQuote(Trim(QPrimary.Fields.AsString[2]));
            if QPrimary.Fields.AsSingle[3] = 0 then
              FOrder := IoAscending else
              FOrder := IoDescending;

            Unk := SQLUnQuote(FName);
          end
        else
          with Uniques[UniquesCount - 1] do
          begin
            SetLength(FFields, FieldsCount + 1);
            FFields[FieldsCount - 1] := FindFieldIndex(Trim(QPrimary.Fields.AsString[1]));
            Include(Fields[FieldsCount - 1].FInfos, fiUnique);
          end;
        QPrimary.Next;
      end;
    end;

  end;

  // Check
  if OIDCheck in OIDs then
  begin
    QCheck.Params.AsString[0] := SQLUnQuote(FName);
    QCheck.Open;
    while not QCheck.Eof do
      with TMetaCheck.Create(Self, Ord(OIDCheck)) do
      begin
        FName := MetaQuote(Trim(QCheck.Fields.AsString[0]));
        QCheck.ReadBlob(1, FConstraint);
        QCheck.Next;
      end;
  end;

  // TRIGGER
  if OIDTableTrigger in OIDs then
  begin
    QTrigger.Params.AsString[0] := SQLUnQuote(FName);
    QTrigger.Open;
    while not QTrigger.Eof do
    begin
      TMetaTrigger.Create(Self, Ord(OIDTableTrigger)).LoadFromQuery(QTrigger);
      QTrigger.Next;
    end;
  end;

  // GRANTS
  if OIDTableGrant in OIDs then
  begin
    QGrants.Params.AsInteger[0] := 0;
    QGrants.Params.AsString[1] := SQLUnQuote(FName);
    QGrants.Open;
    LoadGrantsFromQuery(QGrants);
  end;

  if OIDTableFieldGrant in OIDs then
  begin
    QFieldGrants.Params.AsInteger[0] := 0;
    QFieldGrants.Params.AsString[1] := SQLUnQuote(FName);
    QFieldGrants.Open;
    LoadFieldsGrantsFromQuery(QFieldGrants);
  end;
end;

procedure TMetaTable.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
end;

procedure TMetaTable.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  inherited SaveToStream(Stream);
end;

procedure TMetaTable.SaveToDDL(Stream: TStringStream; options: TDDLOptions);
begin
  inherited SaveToDDL(Stream, options);
  SaveNode(Stream, Ord(OIDPrimary), options);
  SaveNode(Stream, Ord(OIDUnique), options);
  SaveNode(Stream, Ord(OIDIndex), options);
  SaveNode(Stream, Ord(OIDForeign), options);
  SaveNode(Stream, Ord(OIDCheck), options);
  SaveNode(Stream, Ord(OIDTableTrigger), options, NewLine);
  SaveNode(Stream, Ord(OIDTableGrant), options, NewLine);
  SaveNode(Stream, Ord(OIDTableFieldGrant), options, NewLine);
end;

function TMetaTable.GetIndices(const Index: Integer): TMetaIndex;
begin
  Result := TMetaIndex(GetItems(Ord(OIDIndex), Index))
end;

function TMetaTable.GetIndicesCount: Integer;
begin
  Result := FNodeItems[Ord(OIDIndex)].Childs.Count;
end;

function TMetaTable.GetForeign(const Index: Integer): TMetaForeign;
begin
  Result := TMetaForeign(GetItems(Ord(OIDForeign), Index))
end;

function TMetaTable.GetForeignCount: Integer;
begin
  Result := FNodeItems[Ord(OIDForeign)].Childs.Count;
end;

function TMetaTable.FindFieldIndex(const Name: string): Integer;
begin
  for Result := 0 to FieldsCount - 1 do
    if Fields[Result].FName = Name then
      Exit;
  raise EUIBError.CreateFmt(EUIB_FIELDSTRNOTFOUND, [Name]);
end;

function TMetaTable.GetChecks(const Index: Integer): TMetaCheck;
begin
  Result := TMetaCheck(GetItems(Ord(OIDCheck), Index));
end;

function TMetaTable.GetChecksCount: Integer;
begin
  Result := FNodeItems[Ord(OIDCheck)].Childs.Count;
end;

function TMetaTable.GetTriggers(const Index: Integer): TMetaTrigger;
begin
  Result := TMetaTrigger(GetItems(Ord(OIDTableTrigger), Index));
end;

function TMetaTable.GetTriggersCount: Integer;
begin
  Result := FNodeItems[Ord(OIDTableTrigger)].Childs.Count;
end;

procedure TMetaTable.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
var
  I: Integer;
begin
  Stream.WriteString(Format('CREATE TABLE %s (', [Name]));
  for I := 0 to FieldsCount - 1 do
  begin
    Stream.WriteString(NewLine + '   ');
    Fields[I].SaveToDDL(Stream, options);
    if I <> FieldsCount - 1 then
      Stream.WriteString(',');
  end;
  Stream.WriteString(NewLine + ');');
end;

class function TMetaTable.NodeClass: string;
begin
  Result := 'Table';
end;

class function TMetaTable.NodeType: TMetaNodeType;
begin
  Result := MetaTable;
end;

{ TMetaBaseField }

procedure TMetaBaseField.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  Stream.Read(FFieldType, SizeOf(FFieldType));

  if FFieldType = uftNumeric then
  begin
    Stream.Read(FScale, SizeOf(FScale));
    Stream.Read(FPrecision, SizeOf(FPrecision));
  end
  else
  begin
    FScale := 0;
    FPrecision := 0;
  end;

  if FFieldType in [uftChar..uftCstring] then
  begin
    Stream.Read(FLength, SizeOf(FLength));
    ReadString(Stream, FCharSet);
    Stream.Read(FBytesPerCharacter, SizeOf(FBytesPerCharacter));
    ReadString(Stream, FCollation);
  end
  else
  begin
    FLength := 0;
    FCharSet := '';
  end;

  if FFieldType = uftBlob then
  begin
    Stream.Read(FSegmentLength, SizeOf(FSegmentLength));
    Stream.Read(FSubType, SizeOf(FSubType));
  end
  else
  begin
    FSegmentLength := 0;
    FSubType := 0;
  end;

  ReadString(Stream, FDefaultValue);
end;

procedure TMetaBaseField.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  Stream.Write(FFieldType, SizeOf(FFieldType));

  if FFieldType = uftNumeric then
  begin
    Stream.Write(FScale, SizeOf(FScale));
    Stream.Write(FPrecision, SizeOf(FPrecision));
  end;

  if FFieldType in [uftChar..uftCstring] then
  begin
    Stream.Write(FLength, SizeOf(FLength));
    WriteString(Stream, FCharSet);
    Stream.Write(FBytesPerCharacter, SizeOf(FBytesPerCharacter));
    WriteString(Stream, FCollation);
  end;

  if FFieldType = uftBlob then
  begin
    Stream.Write(FSegmentLength, SizeOf(FSegmentLength));
    Stream.Write(FSubType, SizeOf(FSubType));
  end;

  WriteString(Stream, FDefaultValue);
end;

procedure TMetaBaseField.LoadFromQuery(QField, QCharset, QArrayDim: TUIBStatement; DefaultCharset: TCharacterSet);

  procedure FindCharset(const Id: Single; var Charset: string; var Count: Smallint);
  var
    I: Integer;
  begin
    if (QCharset.CurrentState <> qsExecute) then
    begin
      QCharset.Open;
      QCharset.FetchAll;
    end;
    for I := 0 to QCharset.Fields.RecordCount - 1 do
    begin
      QCharset.Fields.GetRecord(I);
      if QCharset.Fields.AsSmallint[0] = Id then
      begin
        Charset := Trim(QCharset.Fields.AsString[1]);
        if not ((id = 3) and (QField.Fields.AsSmallint[11] = 1)) then
          Count := QCharset.Fields.AsSmallint[2] else
          Count := 1;
        Exit;
      end;
      Charset := '';
      FBytesPerCharacter := 1;
    end;
  end;

begin
  FScale := Abs(QField.Fields.AsSmallInt[1]);
  FLength := QField.Fields.AsSmallInt[2];
  FPrecision := QField.Fields.AsSmallInt[3];
  if FScale > 0 then
  begin
    FFieldType := uftNumeric;
    if FPrecision = 0 then
      case QField.Fields.AsSmallint[0] of
        blr_short:
          FPrecision := 4;
        blr_long:
          FPrecision := 7;
        blr_int64, blr_quad, blr_double:
          FPrecision := 15;
      else
        raise EUIBError.Create(EUIB_UNEXPECTEDERROR);
      end;
  end
  else
    case QField.Fields.AsSmallint[0] of
      blr_text, blr_text2:
        FFieldType := uftChar;
      blr_varying, blr_varying2:
        FFieldType := uftVarchar;
      blr_cstring, blr_cstring2:
        FFieldType := uftCstring;
      blr_short:
        FFieldType := uftSmallint;
      blr_long:
        FFieldType := uftInteger;
      blr_quad:
        FFieldType := uftQuad;
      blr_float, blr_d_float:
        FFieldType := uftFloat;
      blr_double:
        FFieldType := uftDoublePrecision;
      blr_timestamp:
        FFieldType := uftTimestamp;
      blr_blob:
        FFieldType := uftBlob;
      blr_blob_id:
        FFieldType := uftBlobId;
      blr_sql_date:
        FFieldType := uftDate;
      blr_sql_time:
        FFieldType := uftTime;
      blr_int64:
        FFieldType := uftInt64;
      {$IFDEF IB7_UP}
      blr_boolean_dtype:
        FFieldType := uftBoolean;
      {$ENDIF IB7_UP}
    end;
  if (FFieldType in [uftChar, uftVarchar, uftCstring]) and
    not QField.Fields.IsNull[4] then
    begin
      FindCharset(QField.Fields.AsSmallint[4], FCharSet, FBytesPerCharacter);
      if (FCharSet = string(CharacterSetStr[DefaultCharset])) then
        FCharSet := '';
      if QField.Fields.IsNull[5] or (QField.Fields.AsInteger[5] = 0) then
        FCollation := ''
      else
        FCollation := Trim(QField.Fields.AsString[6])
    end
  else
    FBytesPerCharacter := 1;

  FSubType := QField.Fields.AsSmallint[7];

  if not QField.Fields.IsNull[8] then
  begin
    QField.ReadBlob(8, FDefaultValue);
    FDefaultValue := Trim(FDefaultValue);
    if FDefaultValue <> '' then
      FDefaultValue := Copy(FDefaultValue, 9, System.Length(FDefaultValue) - 8);
  end
  else
    FDefaultValue := '';
end;

procedure TMetaBaseField.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
begin
  case FFieldType of
    uftNumeric:
      if FSubType = 2 then
        Stream.WriteString(Format('DECIMAL(%d,%d)', [FPrecision, FScale])) else
        Stream.WriteString(Format('NUMERIC(%d,%d)', [FPrecision, FScale]));
    uftChar..uftCstring:
      begin
        Stream.WriteString(Format('%s(%d)',
          [FieldTypes[FFieldType], FLength div FBytesPerCharacter]));
        if FDefaultValue <> '' then
          Stream.WriteString(' DEFAULT ' + FDefaultValue);
        if (FCharSet <> '') then
          Stream.WriteString(' CHARACTER SET ' + FCharSet);
        if (FCollation <> '') then
          Stream.WriteString(' COLLATE ' + FCollation);
      end;
    uftBlob:
      Stream.WriteString(Format('%s SUB_TYPE %d SEGMENT SIZE %d',
        [FieldTypes[FFieldType], FSubType, FSegmentLength]));
  else
    Stream.WriteString(Format('%s', [FieldTypes[FFieldType]]));
  end;

  if (not (FFieldType in [uftChar..uftCstring])) and (FDefaultValue <> '') then
    Stream.WriteString(' DEFAULT ' + FDefaultValue);
end;

class function TMetaBaseField.NodeClass: string;
begin
  Result := 'Field';
end;

function TMetaBaseField.GetShortFieldType: string;
begin
  case FFieldType of
    uftChar..uftCstring:
      Result := Format('%s(%d)', [FieldTypes[FFieldType],
        FLength div FBytesPerCharacter]);
    uftNumeric:
      if FSubType = 2 then
       Result := Format('DECIMAL(%d,%d)', [FPrecision, FScale]) else
       Result := Format('NUMERIC(%d,%d)', [FPrecision, FScale]);
  else
    Result := Format('%s', [FieldTypes[FFieldType]]);
  end;
end;

class function TMetaBaseField.NodeType: TMetaNodeType;
begin
  Result := MetaBaseField;
end;

{ TMetaDataBase }

constructor TMetaDataBase.Create(AOwner: TMetaNode; ClassIndex: Integer);
var
  i: integer;
begin
  inherited Create(nil, -1);

  AddClass(TMetaDomain);
  AddClass(TMetaTable);
  AddClass(TMetaView);
  AddClass(TMetaProcedure);
  AddClass(TMetaGenerator);
  AddClass(TMetaException);
  AddClass(TMetaUDF);
  AddClass(TMetaRole);

  FOIDDatabases := ALLOBjects;
  FOIDTables := ALLTables;
  FOIDViews := ALLViews;
  FOIDProcedures := ALLProcedures;
  FOIDUDFs := ALLUDFs;
  FOIDRoles := ALLRoles;
  FSysInfos := False;
  FDefaultCharset := csNONE;

  FSortedTables := TList.Create;
  FSortedViews := TList.Create;

  FIdentifiers := TAvlStringTree.Create;
  for i := low(SQLToKens) to high(SQLToKens) do
    FIdentifiers.Insert(TAvlString.Create(SQLToKens[i]))
end;

procedure TMetaDataBase.LoadDependencies(QDeps: TUIBStatement);
type
  (* This list was built from the output of the following query

     select
       RDB$TYPE, RDB$TYPE_NAME
     from
       RDB$TYPES
     where
       RDB$FIELD_NAME='RDB$OBJECT_TYPE'
     order by
       RDB$TYPE
  *)
  TObjectType = (otRelation, otView, otTrigger, otComputedField, otValidation,
    otProcedure, otExpressionIndex, otException, otUser, otField, otIndex,
    otDependentCount, otUserGroup, otRole, otGenerator, otUDF, otBlobFilter);
var
  DepName, DepOnName: String;
  DepType, DepOnType: TObjectType;
  DepNode, DepOnNode: TMetaNode;

  function FindNode(const Name: String; ObjectType: TObjectType): TMetaNode;
  begin
    case ObjectType of
      otRelation:
        begin
          Result := FindTableName(Name);
          { Views that depend on views are stored as depending on relations in
            RDB$DEPENDENCIES }
          if Result = nil then
            Result := FindViewName(Name);
        end;
      otView:      Result := FindViewName(Name);
      otTrigger:   Result := FindTriggerName(Name);
      otProcedure: Result := FindProcName(Name);
      otException: Result := FindExceptionName(Name);
      otField:     Result := FindDomainName(Name);
      otGenerator: Result := FindGeneratorName(Name);
      otUDF:       Result := FindUDFName(Name);
    else
      Result := nil;
    end;
  end;

begin
  DepName := Trim(QDeps.Fields.AsString[0]);
  DepType := TObjectType(QDeps.Fields.AsInteger[1]);
  DepOnName := Trim(QDeps.Fields.AsString[2]);
  DepOnType := TObjectType(QDeps.Fields.AsInteger[3]);

  DepNode := FindNode(DepName, DepType);
  DepOnNode := FindNode(DepOnName, DepOnType);

  if (DepNode <> nil) and (DepOnNode <> nil) then
  begin
    DepNode.RegisterDependedOn(DepOnNode);
    DepOnNode.RegisterDependent(DepNode);
  end;
end;

procedure TMetaDataBase.LoadFromDatabase(Transaction: TUIBTransaction);
var
  I: Integer;
  ConStr, Str: string;
  QNames, QFields, QCharset, QPrimary: TUIBStatement;
  QIndex, QForeign, QCheck, QTrigger, QArrayDim: TUIBStatement;
  QDependencies: TUIBStatement;
  QDefaultCharset, QGrants, QFieldGrants: TUIBStatement;
  procedure Configure(var Q: TUIBStatement; const Qry: string;
    CachedFetch: Boolean = False);
  begin
    Q := TUIBStatement.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
    Q.Transaction := Transaction;
    Q.CachedFetch := CachedFetch;
    Q.SQL.Text := Qry;
  end;

begin
  CheckTransaction(Transaction);

  FName := Transaction.DataBase.DatabaseName;

  Configure(QNames, '');
  if FSysInfos then
    Configure(QTrigger, QRYSysTrigger) else
    Configure(QTrigger, QRYTrigger);

  Configure(QCharset, QRYCharset, True);
  Configure(QFields, QRYTableFields);
  Configure(QPrimary, QRYUnique, True);
  Configure(QIndex, QRYIndex);
  Configure(QForeign, QRYForeign);
  Configure(QCheck, QRYCheck);
  Configure(QArrayDim, QRYArrayDim);
  Configure(QDefaultCharset, QRYDefaultCharset);
  Configure(QGrants, QRYRelationGrants);
  Configure(QFieldGrants, QRYFieldGrants);
  Configure(QDependencies, QRYDependendies);

  try
    if OIDDBCharset in FOIDDatabases then
    begin
      QDefaultCharset.Open;
      FDefaultCharset := StrToCharacterSet(Trim(QDefaultCharset.Fields.AsAnsiString[0]));
      QDefaultCharset.Close(etmStayIn);
    end;

    // DOMAINS
    if OIDDomain in FOIDDatabases then
    begin
      FNodeItems[Ord(OIDDomain)].Childs.Clear;
      if FSysInfos then
        QNames.SQL.Text := QRYSysDomains
      else
        QNames.SQL.Text := QRYDomains;
      QNames.Open;
      while not QNames.Eof do
      begin
        with TMetaDomain.Create(Self, Ord(OIDDomain)) do
          LoadFromQuery(QNames, QCharset, QArrayDim, FDefaultCharset);
        QNames.Next;
      end;
    end;

    // GENERATORS
    if OIDGenerator in FOIDDatabases then
    begin
      FNodeItems[Ord(OIDGenerator)].Childs.Clear;
      QNames.SQL.Text := QRYGenerators;
      QNames.Open;
      while not QNames.Eof do
      begin
        with TMetaGenerator.Create(Self, Ord(OIDGenerator)) do
          LoadFromDataBase(Transaction, Trim(QNames.Fields.AsString[0]));
        QNames.Next;
      end;
    end;

    // TABLES
    if OIDTable in FOIDDatabases then
    begin
      FNodeItems[Ord(OIDTable)].Childs.Clear;
      if FSysInfos then
        QNames.SQL.Text := QRYSysTables
      else
        QNames.SQL.Text := QRYTables;
      QNames.Open;
      while not QNames.Eof do
      begin
        with TMetaTable.Create(Self, Ord(OIDTable)) do
          LoadFromDataBase(QNames, QFields, QCharset, QPrimary,
            QIndex, QCheck, QTrigger, QArrayDim, QGrants, QFieldGrants,
            FOIDTables, FDefaultCharset);
        QNames.Next;
      end;

      // FOREIGN
      if [OIDForeign, OIDTableField] <= FOIDTables then
      begin
        for I := 0 to TablesCount - 1 do
        begin
          QForeign.Params.AsString[0] := Tables[I].Name;
          QForeign.Open;
          ConStr := '';
          while not QForeign.Eof do
          begin
            if ConStr <> Trim(QForeign.Fields.AsString[0]) then // new
            begin
              with TMetaForeign.Create(Tables[I], Ord(OIDForeign)) do
              begin
                ConStr := Trim(QForeign.Fields.AsString[0]);
                FName := MetaQuote(ConStr);
                FForTable := FindTableIndex(Trim(QForeign.Fields.AsString[3]));
                SetLength(FFields, 1);
                FFields[0] := Tables[I].FindFieldIndex(Trim(QForeign.Fields.AsString[5]));
                Include(Tables[I].Fields[FFields[0]].FInfos, fiForeign);
                SetLength(FForFields, 1);
                FForFields[0] := ForTable.FindFieldIndex(Trim(QForeign.Fields.AsString[4]));

                Str := Trim(QForeign.Fields.AsString[1]);
                if Str = 'RESTRICT'  then FOnUpdate := urRestrict else
                if Str = 'CASCADE'   then FOnUpdate := urCascade  else
                if Str = 'SET NULL'  then FOnUpdate := urSetNull  else
                if Str = 'NO ACTION' then FOnUpdate := urNoAction else
                  FOnUpdate := urSetDefault;

                Str := Trim(QForeign.Fields.AsString[2]);
                if Str = 'RESTRICT'  then FOnDelete := urRestrict else
                if Str = 'CASCADE'   then FOnDelete := urCascade  else
                if Str = 'SET NULL'  then FOnDelete := urSetNull  else
                if Str = 'NO ACTION' then FOnDelete := urNoAction else
                  FOnDelete := urSetDefault;

                // FB15up
                FIndexName := MetaQuote(Trim(QForeign.Fields.AsString[6]));
                if QForeign.Fields.AsSingle[7] = 0 then
                  FOrder := IoAscending else
                  FOrder := IoDescending;
              end;
            end
            else
              with Tables[I].Foreign[Tables[I].ForeignCount - 1] do
              begin
                SetLength(FFields, Length(FFields) + 1);
                FFields[FieldsCount - 1] := Tables[I].FindFieldIndex(Trim(QForeign.Fields.AsString[5]));
                Include(Tables[I].Fields[FFields[FieldsCount - 1]].FInfos, fiForeign);
                SetLength(FForFields, Length(FForFields) + 1);
                FForFields[ForFieldsCount - 1] := ForTable.FindFieldIndex(Trim(QForeign.Fields.AsString[4]));
              end;
            QForeign.Next;
          end;
        end;
      end;
    end;

    // VIEWS
    if OIDView in FOIDDatabases then
    begin
      FNodeItems[Ord(OIDView)].Childs.Clear;
      QNames.SQL.Text := QRYView;
      QNames.Open;
      while not QNames.Eof do
      begin
        with TMetaView.Create(Self, Ord(OIDView)) do
          LoadFromDataBase(QNames, QFields, QTrigger, QCharset, QArrayDim,
            QGrants, QFieldGrants, FOIDViews, FDefaultCharset);
        QNames.Next;
      end;
    end;

    // PROCEDURE
    if OIDProcedure in FOIDDatabases then
    begin
      FNodeItems[Ord(OIDProcedure)].Childs.Clear;
      QNames.SQL.Text := QRYProcedures;
      QFields.SQL.Text := QRYProcFields;
      QNames.Open;
      while not QNames.Eof do
      begin
        with TMetaProcedure.Create(Self, Ord(OIDProcedure)) do
          LoadFromQuery(QNames, QFields, QCharset, QArrayDim, QGrants, FOIDProcedures, FDefaultCharset);
        QNames.Next;
      end;
    end;

    // EXCEPTION
    if OIDException in FOIDDatabases then
    begin
      FNodeItems[Ord(OIDException)].Childs.Clear;
      QNames.SQL.Text := QRYExceptions;
      QNames.Open;
      while not QNames.Eof do
      begin
        with TMetaException.Create(Self, Ord(OIDException)) do
          LoadFromQuery(QNames);
        QNames.Next;
      end;
    end;

    // UDF
    if OIDUDF in FOIDDatabases then
    begin
      FNodeItems[Ord(OIDUDF)].Childs.Clear;
      QNames.SQL.Text := QRYUDF;
      QFields.SQL.Text := QRYUDFFields;
      QNames.Open;
      while not QNames.Eof do
      begin
        with TMetaUDF.Create(Self, Ord(OIDUDF)) do
          LoadFromQuery(QNames, QFields, QCharset, QArrayDim, FOIDUDFs, FDefaultCharset);
        QNames.Next;
      end;
    end;

    // ROLES
    if OIDRole in FOIDDatabases then
    begin
      FNodeItems[Ord(OIDRole)].Childs.Clear;
      QNames.SQL.Text := QRYRoles;
      QNames.Open;
      while not QNames.Eof do
      begin
        with TMetaRole.Create(Self, Ord(OIDRole)) do
          LoadFromQuery(QNames, QGrants, FOIDRoles);
        QNames.Next;
      end;
    end;

    // DEPENDENCIES
    if OIDDependencies in FOIDDatabases then
    begin
      QDependencies.Open;
      while not QDependencies.Eof do
      begin
        LoadDependencies(QDependencies);
        QDependencies.Next;
      end;
    end;
  finally
    QDependencies.Free;
    QFieldGrants.Free;
    QGrants.Free;
    QNames.Free;
    QCharset.Free;
    QFields.Free;
    QPrimary.Free;
    QIndex.Free;
    QForeign.Free;
    QCheck.Free;
    QTrigger.Free;
    QArrayDim.Free;
    QDefaultCharset.Free;
  end;
end;

procedure TMetaDataBase.SaveToDDL(Stream: TStringStream; options: TDDLOptions);
var
  I: Integer;

  procedure SaveMainNodes(Comment: String; OID: Integer; Separator: String = '');
  var
    I: Integer;
  begin
    Stream.WriteString(Format('/* %s */%s', [comment,NewLine]));
    if FNodeItems[OID].Childs.Count > 0 then
    begin
      for I := 0 to FNodeItems[OID].Childs.Count - 1 do
      begin
        if GetItems(OID, I) is TMetaProcedure then
          TMetaProcedure(GetItems(OID, I)).SaveToCreateEmptyDDL(Stream, options)
        else
          GetItems(OID, I).SaveToDDLNode(Stream, options);
        Stream.WriteString(NewLine + Separator);
      end;

      if Separator = '' then
        Stream.WriteString(NewLine);
    end
    else
      Stream.WriteString(NewLine);
  end;

  procedure SaveChildNodes(comment: string; OIDParent, OIDChild: Integer;
    Separator: String = '');
  var
    I, J: Integer;
    Done: Integer;
  begin
    if FNodeItems[OIDParent].Childs.Count > 0 then
    begin
      Done := 0;
      Stream.WriteString(Format('/* %s */%s', [comment,NewLine]));
      for I := 0 to FNodeItems[OIDParent].Childs.Count - 1 do
        for J := 0 to GetItems(OIDParent, I).FNodeItems[OIDChild].Childs.Count - 1 do
        begin
          TMetaNode(GetItems(OIDParent, I).FNodeItems[OIDChild].Childs[J]).SaveToDDL(Stream, options);
          Stream.WriteString(NewLine + Separator);
          Inc(Done);
        end;
      if (Done = 0) or (Separator = '') then
        Stream.WriteString(NewLine);
    end;
  end;

begin
  SaveMainNodes('ROLES',              Ord(OIDRole));
  SaveMainNodes('FUNCTIONS',          Ord(OIDUDF),       NewLine);
  SaveMainNodes('DOMAINS',            Ord(OIDDomain));
  SaveMainNodes('GENERATORS',         Ord(OIDGenerator), NewLine);
  SaveMainNodes('EXCEPTIONS',         Ord(OIDException));
  SaveMainNodes('PROCEDURES (Empty)', Ord(OIDProcedure), NewLine);
  SaveMainNodes('TABLES',             Ord(OIDTable),     NewLine);
  SaveMainNodes('VIEWS',              Ord(OIDView),      NewLine);

  SaveChildNodes('UNIQUE CONSTRAINTS',Ord(OIDTable), Ord(OIDUnique));
  SaveChildNodes('PRIMARY KEYS',      Ord(OIDTable), Ord(OIDPrimary));
  SaveChildNodes('FOREIGN KEYS',      Ord(OIDTable), Ord(OIDForeign));
  SaveChildNodes('INDICES',           Ord(OIDTable), Ord(OIDIndex));
  SaveChildNodes('CHECK CONSTRAINTS', Ord(OIDTable), Ord(OIDCheck));
  SaveChildNodes('TABLES TRIGGERS',   Ord(OIDTable), Ord(OIDTableTrigger), NewLine);
  SaveChildNodes('VIEWS TRIGGERS',    Ord(OIDView),  Ord(OIDViewTrigers),  NewLine);

  if ProceduresCount > 0 then
  begin
    Stream.WriteString('/* PROCEDURES (Code) */' + NewLine);
    for I := 0 to ProceduresCount - 1 do
    begin
      Procedures[I].SaveToAlterDDL(Stream, options);
      Stream.WriteString(NewLine + NewLine);
    end;
  end;

  { Grants }
  SaveChildNodes('GRANTS (Roles)',         Ord(OIDRole),      Ord(OIDRoleGrant));
  SaveChildNodes('GRANTS (Tables)',        Ord(OIDTable),     Ord(OIDTableGrant));
  SaveChildNodes('GRANTS (Tables Fields)', Ord(OIDTable),     Ord(OIDTableFieldGrant));
  SaveChildNodes('GRANTS (Views)',         Ord(OIDView),      Ord(OIDViewGrant));
  SaveChildNodes('GRANTS (Views Fields)',  Ord(OIDView),      Ord(OIDViewFieldGrant));
  SaveChildNodes('GRANTS (Procedures)',    Ord(OIDProcedure), Ord(OIDProcedureGrant));
end;

function TMetaDataBase.GetGenerators(const Index: Integer): TMetaGenerator;
begin
  Result := TMetaGenerator(GetItems(Ord(OIDGenerator), Index));
end;

function TMetaDataBase.GetGeneratorsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDGenerator)].Childs.Count
end;

function TMetaDataBase.GetTables(const Index: Integer): TMetaTable;
begin
  Result := TMetaTable(GetItems(Ord(OIDTable), Index));
end;

function TMetaDataBase.GetTablesCount: Integer;
begin
  Result := FNodeItems[Ord(OIDTable)].Childs.Count
end;

function TMetaDataBase.FindTableName(const TableName: string): TMetaTable;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to TablesCount - 1 do
    if CompareText(Tables[I].Name, TableName) = 0 then
    begin
      Result := Tables[I];
      Exit;
    end;
end;

function TMetaDataBase.FindTableIndex(const TableName: string): Integer;
begin
  for Result := 0 to TablesCount - 1 do
    if Tables[Result].Name = TableName then
      Exit;
  raise Exception.CreateFmt(EUIB_TABLESTRNOTFOUND, [TableName]);
end;

destructor TMetaDataBase.Destroy;
begin
  FSortedTables.Free;
  FSortedViews.Free;
  FIdentifiers.Free;
  inherited;
end;

function TMetaDataBase.FindDomainIndex(const DomainName: string): Integer;
begin
  for Result := 0 to DomainsCount - 1 do
    if Domains[Result].Name = DomainName then
      Exit;
  raise Exception.CreateFmt(EUIB_DOMAINSTRNOTFOUND, [DomainName]);
end;

procedure TMetaDataBase.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  Stream.Read(FDefaultCharset, SizeOf(FDefaultCharset));
end;

function TMetaDataBase.GetViews(const Index: Integer): TMetaView;
begin
  Result := TMetaView(GetItems(Ord(OIDView), Index));
end;

function TMetaDataBase.GetViewsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDView)].Childs.Count
end;

function TMetaDataBase.GetDomains(const Index: Integer): TMetaDomain;
begin
  Result := TMetaDomain(GetItems(Ord(OIDDomain), Index));
end;

function TMetaDataBase.GetDomainsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDDomain)].Childs.Count
end;

function TMetaDataBase.GetProcedures(const Index: Integer): TMetaProcedure;
begin
  Result := TMetaProcedure(GetItems(Ord(OIDProcedure), Index));
end;

function TMetaDataBase.GetProceduresCount: Integer;
begin
  Result := FNodeItems[Ord(OIDProcedure)].Childs.Count
end;

function TMetaDataBase.GetExceptions(const Index: Integer): TMetaException;
begin
  Result := TMetaException(GetItems(Ord(OIDException), Index));
end;

function TMetaDataBase.GetExceptionsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDException)].Childs.Count
end;

function TMetaDataBase.GetUDFS(const Index: Integer): TMetaUDF;
begin
  Result := TMetaUDF(GetItems(Ord(OIDUDF), Index));
end;

function TMetaDataBase.GetUDFSCount: Integer;
begin
  Result := FNodeItems[Ord(OIDUDF)].Childs.Count
end;

class function TMetaDataBase.NodeClass: string;
begin
  Result := 'Database';
end;

procedure TMetaDataBase.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  Stream.WriteBuffer(FDefaultCharset, SizeOf(FDefaultCharset));
  inherited SaveToStream(Stream);
end;

procedure TMetaDataBase.SortTablesByForeignKeys;
var
  I, F, O: Integer;
  CanAdd: Boolean;
  ToAdd: TList;
  Current: TMetaTable;
begin
  FSortedTables.Clear;

  { Commence par ajouter les tables qui n'ont pas de dépendences }
  ToAdd := TList.Create;
  try
    for I := 0 to GetTablesCount - 1 do
    begin
      if GetTables(i).ForeignCount = 0 then
        FSortedTables.Add(GetTables(i))
      else
        ToAdd.Add(GetTables(i));
    end;

    { Ajoute ensuite les tables qui ont uniquement des dépendences sur les tables
      qui sont déjà dans la liste }
    I := 0; O := 0;
    while ToAdd.Count > 0 do
    begin
      { Boucle sur la liste ToAdd }
      if I >= ToAdd.Count then
      begin
        if (O > 0) and (ToAdd.Count = O) then
          raise EUIBError.Create('Cycle detected, I can''t do anything for bad databases designers :-)')
        else
          O := ToAdd.Count;

        I := 0;
      end;

      Current := TMetaTable(ToAdd[I]);

      CanAdd := true;
      for F := 0 to Current.ForeignCount - 1 do
      begin
        { Il faut que toutes les dépendances de Current soient dans
          FSortedTables. Ne tient pas compte des jointures auto-réflexives }
        if (Current.Foreign[F].ForTable <> Current)
          and (FSortedTables.IndexOf(Current.Foreign[F].ForTable) < 0) then
        begin
          CanAdd := false;
          Break;
        end;
      end;
      if CanAdd then
      begin
        FSortedTables.Add(Current);
        ToAdd.Remove(Current);
        { Ne change pas I, comme on a supprimé un item, la même valeur de I
          permettra d'avoir le suivant dans la liste }
      end
      else
        Inc(I);
    end;
  finally
    ToAdd.Free;
  end;
end;

procedure TMetaDataBase.SortViewsByDependencies;
var
  I, F, O: Integer;
  CanAdd: Boolean;
  ToAdd: TList;
  Current: TMetaView;

  function CountViewsDependencies(V: TMetaView): Integer;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 0 to V.DependedOnCount - 1 do
      if V.DependedOn[i].NodeType = MetaView then
        Inc(Result);
  end;

begin
  FSortedViews.Clear;

  { Commence par ajouter les tables qui n'ont pas de dépendences }
  ToAdd := TList.Create;
  try
    for I := 0 to GetViewsCount - 1 do
    begin
      if CountViewsDependencies(GetViews(i)) = 0 then
        FSortedViews.Add(GetViews(i))
      else
        ToAdd.Add(GetViews(i));
    end;

    { Ajoute ensuite les tables qui ont uniquement des dépendences sur les tables
      qui sont déjà dans la liste }
    I := 0; O := 0;
    while ToAdd.Count > 0 do
    begin
      { Boucle sur la liste ToAdd }
      if I >= ToAdd.Count then
      begin
        if (O > 0) and (ToAdd.Count = O) then
          raise EUIBError.Create('Cycle detected, I can''t do anything for bad databases designers :-)')
        else
          O := ToAdd.Count;

        I := 0;
      end;

      Current := TMetaView(ToAdd[I]);

      CanAdd := true;
      for F := 0 to Current.DependedOnCount - 1 do
      begin
        { Il faut que toutes les dépendances de Current soient dans
          FSortedTables. Ne tient pas compte des jointures auto-réflexives }
        if (Current.DependedOn[F].NodeType = MetaView) and (Current.DependedOn[F] <> Current)
          and (FSortedViews.IndexOf(Current.DependedOn[F]) < 0) then
        begin
          CanAdd := false;
          Break;
        end;
      end;
      if CanAdd then
      begin
        FSortedViews.Add(Current);
        ToAdd.Remove(Current);
        { Ne change pas I, comme on a supprimé un item, la même valeur de I
          permettra d'avoir le suivant dans la liste }
      end
      else
        Inc(I);
    end;
  finally
    ToAdd.Free;
  end;
end;

function TMetaDataBase.GetRoles(const Index: Integer): TMetaRole;
begin
  Result := TMetaRole(GetItems(Ord(OIDRole), Index));
end;

function TMetaDataBase.GetRolesCount: Integer;
begin
  Result := FNodeItems[Ord(OIDRole)].Childs.Count
end;

function TMetaDataBase.GetSortedTables(const Index: Integer): TMetaTable;
begin
  if FSortedTables.Count <> GetTablesCount then
    SortTablesByForeignKeys;
  Result := TMetaTable(FSortedTables[Index]);
end;

function TMetaDataBase.GetSortedTablesCount: Integer;
begin
  if FSortedTables.Count <> GetTablesCount then
    SortTablesByForeignKeys;
  Result := FSortedTables.Count;
end;

function TMetaDataBase.GetSortedViews(const Index: Integer): TMetaView;
begin
  if FSortedViews.Count <> GetViewsCount then
    SortViewsByDependencies;
  Result := TMetaView(FSortedViews[Index]);
end;

function TMetaDataBase.GetSortedViewsCount: Integer;
begin
  if FSortedViews.Count <> GetViewsCount then
    SortViewsByDependencies;
  Result := FSortedViews.Count;
end;

class function TMetaDataBase.NodeType: TMetaNodeType;
begin
  Result := MetaDatabase;
end;

function TMetaDataBase.FindProcName(const ProcName: string): TMetaProcedure;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ProceduresCount - 1 do
    if CompareText(Procedures[I].Name, ProcName) = 0 then
    begin
      Result := Procedures[I];
      Exit;
    end;
end;

function TMetaDataBase.FindExceptionName(
  const ExcepName: string): TMetaException;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ExceptionsCount - 1 do
    if CompareText(Exceptions[I].Name, ExcepName) = 0 then
    begin
      Result := Exceptions[I];
      Exit;
    end;
end;

function TMetaDataBase.FindGeneratorName(
  const GenName: string): TMetaGenerator;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to GeneratorsCount - 1 do
    if CompareText(Generators[I].Name, GenName) = 0 then
    begin
      Result := Generators[I];
      Exit;
    end;
end;

function TMetaDataBase.FindUDFName(const UDFName: string): TMetaUDF;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to UDFSCount - 1 do
    if CompareText(UDFS[I].FName, UDFName) = 0 then
    begin
      Result := UDFS[I];
      Exit;
    end;
end;

function TMetaDataBase.FindViewName(const ViewName: string): TMetaView;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ViewsCount - 1 do
    if CompareText(Views[I].Name, ViewName) = 0 then
    begin
      Result := Views[I];
      Exit;
    end;
end;

function TMetaDataBase.FindRoleName(const RoleName: string): TMetaRole;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to RolesCount - 1 do
    if CompareText(Roles[I].Name, RoleName) = 0 then
    begin
      Result := Roles[I];
      Exit;
    end;
end;

function TMetaDataBase.FindDomainName(
  const DomainName: string): TMetaDomain;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to DomainsCount - 1 do
    if CompareText(Domains[I].Name, DomainName) = 0 then
    begin
      Result := Domains[I];
      Exit;
    end;
end;

function TMetaDataBase.FindTriggerName(
  const TriggerName: string): TMetaTrigger;
var
  i, j: Integer;
  table: TMetaTable;
begin
  Result := nil;
  for i := 0 to TablesCount - 1 do
  begin
    table := Tables[i];
    for j := 0 to table.TriggersCount - 1 do
      if CompareText(table.Triggers[j].Name, TriggerName) = 0 then
      begin
        Result := table.Triggers[j];
        Exit;
      end;
  end;
end;

{ TMetaConstraint }

function TMetaConstraint.GetFields(const Index: Word): TMetaTableField;
begin
  Assert((FieldsCount > 0) and (Index < FieldsCount), IntToStr(Index) + ' ' + ClassName);
  Result := TMetaTable(FOwner).Fields[FFields[Index]];
end;

function TMetaConstraint.GetFieldsCount: Word;
begin
  Result := Length(FFields);
end;

class function TMetaConstraint.NodeClass: string;
begin
  Result := 'Constraint'
end;

procedure TMetaConstraint.LoadFromStream(Stream: TStream);
var
  I: Integer;
begin
  Stream.Read(I, SizeOf(I));
  SetLength(FFields, I);
  if I > 0 then
  begin
    ReadString(Stream, FName);
    for I := 0 to I - 1 do
    begin
      Stream.Read(FFields[I], SizeOf(FFields[I]));
      case NodeType of
        MetaForeign:
          Include(TMetaTable(FOwner).Fields[FFields[I]].FInfos, fiForeign);
        MetaIndex:
          Include(TMetaTable(FOwner).Fields[FFields[I]].FInfos, fiIndice);
        MetaPrimary:
          Include(TMetaTable(FOwner).Fields[FFields[I]].FInfos, fiPrimary);
        MetaUnique:
          Include(TMetaTable(FOwner).Fields[FFields[I]].FInfos, fiPrimary);
      end;
    end;
  end;
  Stream.Read(FOrder, SizeOf(FOrder));
end;

procedure TMetaConstraint.SaveToStream(Stream: TStream);
var
  I: Integer;
begin
  I := FieldsCount;
  Stream.Write(I, SizeOf(I));
  if I > 0 then
  begin
    WriteString(Stream, FName);
    for I := 0 to I - 1 do
      Stream.Write(FFields[I], SizeOf(FFields[I]));
  end;
  Stream.Write(FOrder, SizeOf(FOrder));
end;

class function TMetaConstraint.NodeType: TMetaNodeType;
begin
  Result := MetaConstraint;
end;

function TMetaConstraint.GetAsDropDDL: string;
begin
  Result := Format('ALTER TABLE %s DROP CONSTRAINT %s;', [TMetaTable(FOwner).Name, Name]);
end;

{ TMetaUnique }

procedure TMetaUnique.LoadFromStream(Stream: TStream);
begin
  inherited;
  ReadString(Stream, FIndexName);
end;

class function TMetaUnique.NodeClass: string;
begin
  Result := 'Unique';
end;

class function TMetaUnique.NodeType: TMetaNodeType;
begin
  Result := MetaUnique;
end;

procedure TMetaUnique.SaveToDDL(Stream: TStringStream; options: TDDLOptions);
var
  I: Integer;
begin
  if (copy(FName, 0, 6) = 'INTEG_') then
    Stream.WriteString(Format('ALTER TABLE %s' + NewLine + '  ADD UNIQUE (',
      [TMetaTable(FOwner).Name])) else
    Stream.WriteString(Format('ALTER TABLE %s' + NewLine + '  ADD CONSTRAINT %s UNIQUE (',
      [TMetaTable(FOwner).Name, Name]));
  for I := 0 to FieldsCount - 1 do
  begin
    Stream.WriteString(Fields[I].Name);
    if I <> FieldsCount - 1 then
      Stream.WriteString(', ');
  end;
  Stream.WriteString(')');

  // fb15up
  if not ((FIndexName = '') or (not (ddlFull in options) and (Copy(FIndexName, 1, 4) = 'RDB$'))) then
  begin
    Stream.WriteString(NewLine + '  USING');
    if FOrder = ioDescending then
      Stream.WriteString(' DESC');
    Stream.WriteString(' INDEX ' + FIndexName);
  end;

  Stream.WriteString(';');
end;

procedure TMetaUnique.SaveToStream(Stream: TStream);
begin
  inherited;
  WriteString(Stream, FIndexName);
end;

function TMetaUnique.GetAsAlterToActiveDDL: string;
begin
  Result := Format('ALTER INDEX %s ACTIVE;', [FIndexName]);
end;

function TMetaUnique.GetAsAlterToInactiveDDL: string;
begin
  Result := Format('ALTER INDEX %s INACTIVE;', [FIndexName]);
end;

{ TMetaPrimary }

procedure TMetaPrimary.LoadFromStream(Stream: TStream);
begin
  inherited;
  ReadString(Stream, FIndexName);
end;

class function TMetaPrimary.NodeClass: string;
begin
  Result := 'Primary key';
end;

procedure TMetaPrimary.LoadFromQuery(Q: TUIBStatement);
var
  I: Integer;
begin
  FName := MetaQuote(Trim(Q.Fields.AsString[0]));
  Q.FetchAll;
  SetLength(FFields, Q.Fields.RecordCount);
  for I := 0 to Q.Fields.RecordCount - 1 do
  begin
    Q.Fields.GetRecord(I);
    FFields[I] := TMetaTable(FOwner).FindFieldIndex(Trim(Q.Fields.AsString[1]));
    Include(TMetaTable(FOwner).Fields[FFields[I]].FInfos, fiPrimary);
  end;

  // FB15up
  FIndexName := MetaQuote(Trim(Q.Fields.AsString[2]));
  if Q.Fields.AsSingle[3] = 0 then
    FOrder := IoAscending else
    FOrder := IoDescending;
end;

procedure TMetaPrimary.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
var
  I: Integer;
begin
  if (copy(FName, 0, 6) = 'INTEG_') then
    Stream.WriteString(Format('ALTER TABLE %s' + NewLine + '  ADD PRIMARY KEY (',
      [TMetaTable(FOwner).Name]))
  else
    Stream.WriteString(Format('ALTER TABLE %s' + NewLine + '  ADD CONSTRAINT %s PRIMARY KEY (',
      [TMetaTable(FOwner).Name, Name]));
  for I := 0 to FieldsCount - 1 do
  begin
    Stream.WriteString(Fields[I].Name);
    if I <> FieldsCount - 1 then
      Stream.WriteString(', ');
  end;
  Stream.WriteString(')');

  // fb15up
  if not ((FIndexName = '') or (not (ddlFull in options) and (Copy(FIndexName, 1, 4) = 'RDB$'))) then
  begin
    Stream.WriteString(NewLine + '  USING');
    if FOrder = ioDescending then
      Stream.WriteString(' DESC');
    Stream.WriteString(' INDEX ' + FIndexName);
  end;
  Stream.WriteString(';');
end;

procedure TMetaPrimary.SaveToStream(Stream: TStream);
begin
  inherited;
  WriteString(Stream, FIndexName);
end;

class function TMetaPrimary.NodeType: TMetaNodeType;
begin
  Result := MetaPrimary;
end;

{ TMetaIndex }

class function TMetaIndex.NodeClass: string;
begin
  Result := 'Indice';
end;

function TMetaIndex.GetAsAlterDDL: string;
begin
  if FActive then
    Result := GetAsAlterToActiveDDL
  else
    Result := GetAsAlterToInactiveDDL;
end;

function TMetaIndex.GetAsAlterToActiveDDL: string;
begin
  Result := Format('ALTER INDEX %s ACTIVE;', [Name]);
end;

function TMetaIndex.GetAsAlterToInactiveDDL: string;
begin
  Result := Format('ALTER INDEX %s INACTIVE;', [Name]);
end;

procedure TMetaIndex.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  Stream.Read(FUnique, SizeOf(FUnique));
  Stream.Read(FActive, SizeOf(FActive));
{$IFDEF FB21_UP}
  ReadString(Stream, FComputedSource);
{$ENDIF}
end;

procedure TMetaIndex.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
var
  I: Integer;
  UNIQUE, ORDER: string;
begin
  if FUnique then
    UNIQUE := ' UNIQUE';
  if FOrder = IoDescending then
    ORDER := ' DESCENDING';

  Stream.WriteString(Format('CREATE%s%s INDEX %s' + NewLine + '  ON %s ',
    [UNIQUE, ORDER, Name, TMetaTable(FOwner).Name]));

{$IFDEF FB21_UP}
  if FComputedSource <> '' then
    Stream.WriteString(Format(NewLine + '  COMPUTED BY %s;', [FComputedSource]))
  else
  begin
{$ENDIF}
    Stream.WriteString('(');
    for I := 0 to FieldsCount - 1 do
    begin
      Stream.WriteString(Fields[I].Name);
      if I <> FieldsCount - 1 then
        Stream.WriteString(', ');
    end;
    Stream.WriteString(');');
{$IFDEF FB21_UP}
  end;
{$ENDIF}
  if (ddlFull in Options) and (not FActive) then
    Stream.WriteString(Format('%sALTER INDEX %s INACTIVE;', [NewLine, Name]));
end;

procedure TMetaIndex.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(FUnique, SizeOf(FUnique));
  Stream.Write(FActive, SizeOf(FActive));
{$IFDEF FB21_UP}
  WriteString(Stream, FComputedSource);
{$ENDIF}
end;

class function TMetaIndex.NodeType: TMetaNodeType;
begin
  Result := MetaIndex;
end;

{ TMetaForeign }

function TMetaForeign.GetForFields(const Index: Word): TMetaTableField;
begin
  Assert((ForFieldsCount > 0) and (Index < ForFieldsCount));
  Result := ForTable.Fields[FForFields[Index]];
end;

function TMetaForeign.GetForFieldsCount: Word;
begin
  Result := Length(FForFields);
end;

function TMetaForeign.GetForTable: TMetaTable;
begin
  Result := TMetaDataBase(FOwner.FOwner).Tables[FForTable];
end;

class function TMetaForeign.NodeClass: string;
begin
  Result := 'Foreign';
end;

procedure TMetaForeign.LoadFromStream(Stream: TStream);
var
  I: Integer;
begin
  inherited LoadFromStream(Stream);
  Stream.Read(FForTable, SizeOf(FForTable));
  Stream.Read(FOnDelete, SizeOf(FOnDelete));
  Stream.Read(FOnUpdate, SizeOf(FOnUpdate));
  Stream.Read(I, SizeOf(I));
  SetLength(FForFields, I);
  for I := 0 to I - 1 do
    Stream.Read(FForFields[I], SizeOf(FForFields[I]));
  ReadString(Stream, FIndexName);
end;

procedure TMetaForeign.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
var
  I: Integer;
begin
  if (Copy(FName, 0, 6) = 'INTEG_') then
    Stream.WriteString(Format('ALTER TABLE %s' + NewLine +
      '  ADD FOREIGN KEY (', [TMetaTable(FOwner).Name]))
  else
    Stream.WriteString(Format('ALTER TABLE %s' + NewLine +
      '  ADD CONSTRAINT %s' + NewLine +
      '  FOREIGN KEY (', [TMetaTable(FOwner).Name, Name]));

  for I := 0 to FieldsCount - 1 do
  begin
    Stream.WriteString(Fields[I].Name);
    if I <> FieldsCount - 1 then
      Stream.WriteString(', ');
  end;
  Stream.WriteString(Format(')' + NewLine + '  REFERENCES %s (', [ForTable.Name]));
  for I := 0 to ForFieldsCount - 1 do
  begin
    Stream.WriteString(ForFields[I].Name);
    if I <> ForFieldsCount - 1 then
      Stream.WriteString(', ');
  end;
  Stream.WriteString(')');

  case OnDelete of
    urCascade:    Stream.WriteString(NewLine + '  ON DELETE CASCADE');
    urSetNull:    Stream.WriteString(NewLine + '  ON DELETE SET NULL');
    urNoAction:   Stream.WriteString(NewLine + '  ON DELETE NO ACTION');
    urSetDefault: Stream.WriteString(NewLine + '  ON DELETE SET DEFAULT');
  end;

  case OnUpdate of
    urCascade:    Stream.WriteString(NewLine + '  ON UPDATE CASCADE');
    urSetNull:    Stream.WriteString(NewLine + '  ON UPDATE SET NULL');
    urNoAction:   Stream.WriteString(NewLine + '  ON UPDATE NO ACTION');
    urSetDefault: Stream.WriteString(NewLine + '  ON UPDATE SET DEFAULT');
  end;

  // fb15up
  if not ((FIndexName = '') or (not (ddlFull in options) and (Copy(FIndexName, 1, 4) = 'RDB$'))) then
  begin
    Stream.WriteString(NewLine + '  USING');
    if FOrder = ioDescending then
      Stream.WriteString(' DESC');
    Stream.WriteString(' INDEX ' + FIndexName);
  end;

  Stream.WriteString(';');
end;

procedure TMetaForeign.SaveToStream(Stream: TStream);
var
  I: Integer;
begin
  inherited SaveToStream(Stream);
  Stream.Write(FForTable, SizeOf(FForTable));
  Stream.Write(FOnDelete, SizeOf(FOnDelete));
  Stream.Write(FOnUpdate, SizeOf(FOnUpdate));
  I := ForFieldsCount;
  Stream.Write(I, SizeOf(I));
  for I := 0 to I - 1 do
    Stream.Write(FForFields[I], SizeOf(FForFields[I]));
  WriteString(Stream, FIndexName);
end;

class function TMetaForeign.NodeType: TMetaNodeType;
begin
  Result := MetaForeign;
end;

{ TMetaCheck }

class function TMetaCheck.NodeClass: string;
begin
  Result := 'Check';
end;

procedure TMetaCheck.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  ReadString(Stream, FConstraint);
end;

procedure TMetaCheck.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
begin
  if (copy(FName, 0, 6) = 'INTEG_') then
    Stream.WriteString(Format('ALTER TABLE %s' + NewLine +
      '  ADD %s;', [TMetaTable(FOwner).Name, FConstraint])) else
    Stream.WriteString(Format('ALTER TABLE %s' + NewLine +
      '  ADD CONSTRAINT %s' + NewLine + '  %s;', [TMetaTable(FOwner).Name, Name, FConstraint]));
end;

procedure TMetaCheck.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  WriteString(Stream, FConstraint);
end;

class function TMetaCheck.NodeType: TMetaNodeType;
begin
  Result := MetaCheck;
end;

{ TMetaTrigger }

class function TMetaTrigger.DecodePrefix(Value: Integer): TTriggerPrefix;
begin
  Result := TTriggerPrefix((Value + 1) and 1);
end;

class function TMetaTrigger.DecodeSuffixes(Value: Integer): TTriggerSuffixes;
var
  V, Slot: Integer;
begin
  Result := [];
  for Slot := 1 to 3 do
  begin
    V := ((Value + 1) shr (Slot * 2 - 1)) and 3;
    if V > 0 then
      Include(Result, TTriggerSuffix(V - 1));
  end;
end;

class function TMetaTrigger.NodeClass: string;
begin
  Result := 'Trigger';
end;

procedure TMetaTrigger.LoadFromQuery(Q: TUIBStatement);
begin
  FName := MetaQuote(Trim(Q.Fields.AsString[0]));
  Q.ReadBlob(1, FSource);
  FPosition := Q.Fields.AsSmallint[2];
  FPrefix := DecodePrefix(Q.Fields.AsSmallint[3]);
  FSuffix := DecodeSuffixes(Q.Fields.AsSmallint[3]);
  FActive := Q.Fields.AsSmallint[4] = 0;
end;

procedure TMetaTrigger.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  Stream.Read(FPrefix, SizeOf(FPrefix));
  Stream.Read(FSuffix, SizeOf(FSuffix));
  Stream.Read(FPosition, SizeOf(FPosition));
  Stream.Read(FActive, SizeOf(FActive));
  ReadString(Stream, FSource);
end;

procedure TMetaTrigger.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
var
  Count: Smallint;
  Suf: TTriggerSuffix;
begin
  Stream.WriteString(Format('CREATE TRIGGER %s FOR %s%s',
    [Name, TMetaNode(FOwner).Name, NewLine]));
  if not FActive then
    Stream.WriteString('INACTIVE ');

  Stream.WriteString(TriggerPrefixTypes[FPrefix] + ' ');
  Count := 0;
  for Suf := tsInsert to tsDelete do
    if Suf in FSuffix then
    begin
      Inc(Count);
      if Count > 1 then
        Stream.WriteString(' OR ');
      Stream.WriteString(TriggerSuffixTypes[Suf]);
    end;
  Stream.WriteString(Format(' POSITION %d%s%s;', [FPosition, NewLine, FSource]));
end;

procedure TMetaTrigger.SaveToAlterToInactiveDDL(Stream: TStringStream);
begin
  Stream.WriteString('ALTER TRIGGER ' + Name + ' INACTIVE;');
end;

procedure TMetaTrigger.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  Stream.Write(FPrefix, SizeOf(FPrefix));
  Stream.Write(FSuffix, SizeOf(FSuffix));
  Stream.Write(FPosition, SizeOf(FPosition));
  Stream.Write(FActive, SizeOf(FActive));
  WriteString(Stream, FSource);
end;

class function TMetaTrigger.NodeType: TMetaNodeType;
begin
  Result := MetaTrigger;
end;

procedure TMetaTrigger.SaveToAlterToActiveDDL(Stream: TStringStream);
begin
  Stream.WriteString('ALTER TRIGGER ' + Name + ' ACTIVE;');
end;

procedure TMetaTrigger.SaveToAlterDDL(Stream: TStringStream);
begin
  Stream.WriteString(Format('ALTER TRIGGER %s%s%s', [Name, NewLine, FSource]));
end;

function TMetaTrigger.GetAsAlterToActiveDDL: string;
var stream: TStringStream;
begin
  stream := TStringStream.Create('');
  try
    SaveToAlterToActiveDDL(stream);
    result := stream.DataString;
  finally
    stream.Free;
  end;
end;

function TMetaTrigger.GetAsAlterDDL: string;
var stream: TStringStream;
begin
  stream := TStringStream.Create('');
  try
    SaveToAlterDDL(stream);
    result := stream.DataString;
  finally
    stream.Free;
  end;
end;

function TMetaTrigger.GetAsAlterToInactiveDDL: string;
var stream: TStringStream;
begin
  stream := TStringStream.Create('');
  try
    SaveToAlterToInactiveDDL(stream);
    result := stream.DataString;
  finally
    stream.Free;
  end;
end;

{ TMetaView }

constructor TMetaView.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  inherited Create(AOwner, ClassIndex);
  AddClass(TMetaField);
  AddClass(TMetaTrigger);
  AddClass(TMetaTableGrant);
  AddClass(TMetaFieldGrant);

  FGrantsClassIndex := Ord(OIDViewGrant);
  FFieldsGrantsClassIndex := Ord(OIDViewFieldGrant);  
end;

function TMetaView.GetFields(const Index: Integer): TMetaField;
begin
  Result := TMetaField(GetItems(Ord(OIDViewField), Index))
end;

function TMetaView.GetFieldsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDViewField)].Childs.Count;
end;

class function TMetaView.NodeClass: string;
begin
  Result := 'View';
end;

function TMetaView.GetTriggers(const Index: Integer): TMetaTrigger;
begin
  Result := TMetaTrigger(GetItems(Ord(OIDViewTrigers), Index))
end;

function TMetaView.GetTriggersCount: Integer;
begin
  Result := FNodeItems[Ord(OIDViewTrigers)].Childs.Count;
end;

procedure TMetaView.LoadFromDataBase(QName, QFields, QTriggers,
  QCharset, QArrayDim, QGrants, QFieldGrants: TUIBStatement; OIDs: TOIDViews; DefaultCharset: TCharacterSet);
begin
  FName := MetaQuote(Trim(QName.Fields.AsString[0]));
  QName.ReadBlob(1, FSource);
  FSource := Trim(FSource);

  // FIELD
  if OIDViewField in OIDs then
  begin
    QFields.Params.AsString[0] := SQLUnQuote(FName);
    QFields.Open;
    while not QFields.Eof do
    begin
      TMetaField.Create(Self, Ord(OIDViewField)).LoadFromQuery(QFields, QCharset, QArrayDim, DefaultCharset);
      QFields.Next;
    end;
  end;

  // TRIGGER
  if OIDViewTrigers in OIDs then
  begin
    QTriggers.Params.AsString[0] := SQLUnQuote(FName);
    QTriggers.Open;
    while not QTriggers.Eof do
    begin
      TMetaTrigger.Create(Self, Ord(OIDViewTrigers)).LoadFromQuery(QTriggers);
      QTriggers.Next;
    end;
  end;

  // GRANTS
  if OIDViewGrant in OIDs then
  begin
    QGrants.Params.AsInteger[0] := 0;
    QGrants.Params.AsString[1] := SQLUnQuote(FName);
    QGrants.Open;
    LoadGrantsFromQuery(QGrants);
  end;

  // FIELD GRANTS
  if OIDViewFieldGrant in OIDs then
  begin
    QFieldGrants.Params.AsInteger[0] := 0;
    QFieldGrants.Params.AsString[1] := SQLUnQuote(FName);
    QFieldGrants.Open;

    LoadFieldsGrantsFromQuery(QFieldGrants);
  end;
end;

procedure TMetaView.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  ReadString(Stream, FSource);
end;

procedure TMetaView.SaveToDDL(Stream: TStringStream; options: TDDLOptions);
begin
  inherited SaveToDDL(Stream, options);
  SaveNode(Stream, Ord(OIDViewTrigers), options, NewLine);
end;

procedure TMetaView.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
var
  I: Integer;
begin
  Stream.WriteString(Format({$IFDEF FB15_UP}'RECREATE'{$ELSE}'CREATE'{$ENDIF} + ' VIEW %s (', [Name]));
  for I := 0 to FieldsCount - 1 do
  begin
    Stream.WriteString(NewLine + '   ' + Fields[I].Name);
    if I <> FieldsCount - 1 then
      Stream.WriteString(',');
  end;
  Stream.WriteString(NewLine + ')' + NewLine + 'AS' + NewLine);
  Stream.WriteString(Source + NewLine);
  Stream.WriteString(';');
end;

procedure TMetaView.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  WriteString(Stream, FSource);
  inherited SaveToStream(Stream);
end;

class function TMetaView.NodeType: TMetaNodeType;
begin
  Result := MetaView;
end;

function TMetaView.GetAsDropDDL: string;
begin
  Result := 'DROP VIEW ' + FName + ';';
end;

{ TMetaDomain }

class function TMetaDomain.NodeClass: string;
begin
  Result := 'Domain';
end;

class function TMetaDomain.NodeType: TMetaNodeType;
begin
  Result := MetaDomain;
end;

procedure TMetaDomain.SaveToDDL(Stream: TStringStream; options: TDDLOptions);
begin
  SaveToDDLNode(Stream, options);
end;

procedure TMetaDomain.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
begin
  Stream.WriteString(Format('CREATE DOMAIN %s AS ', [Name]));
  inherited SaveToDDLNode(Stream, options);
  Stream.WriteString(';');
end;

{ TMetaProcedure }

constructor TMetaProcedure.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  inherited Create(AOwner, ClassIndex);
  AddClass(TMetaProcInField); // in
  AddClass(TMetaProcOutField); // out
  AddClass(TMetaProcedureGrant); // grants
end;

function TMetaProcedure.GetInputFields(const Index: Integer): TMetaProcInField;
begin
  Result := TMetaProcInField(GetItems(Ord(OIDProcFieldIn), Index))
end;

function TMetaProcedure.GetInputFieldsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDProcFieldIn)].Childs.Count;
end;

class function TMetaProcedure.NodeClass: string;
begin
  Result := 'Procedure';
end;

function TMetaProcedure.GetOutputFields(const Index: Integer): TMetaProcOutField;
begin
  Result := TMetaProcOutField(GetItems(Ord(OIDProcFieldOut), Index))
end;

function TMetaProcedure.GetOutputFieldsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDProcFieldOut)].Childs.Count;
end;

function TMetaProcedure.GetProcedureGrants(const Index: Integer): TMetaProcedureGrant;
begin
  Result := TMetaProcedureGrant(GetItems(Ord(OIDProcedureGrant),Index));
end;

function TMetaProcedure.GetProcedureGrantsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDProcedureGrant)].Childs.Count;
end;

procedure TMetaProcedure.InternalSaveToDDL(Stream: TStringStream;
  Operation: string; options: TDDLOptions);
var
  I: Integer;
begin
  Stream.WriteString(Format('%s PROCEDURE %s', [Operation, Name]));
  if InputFieldsCount > 0 then
  begin
    Stream.WriteString(' (');
    for I := 0 to InPutFieldsCount - 1 do
    begin
      Stream.WriteString(NewLine + '   ');
      InputFields[I].SaveToDDL(Stream, options);
      if I <> InputFieldsCount - 1 then
        Stream.WriteString(',');
    end;
    Stream.WriteString(')');
  end;

  if OutputFieldsCount > 0 then
  begin
    Stream.WriteString(Format('%sRETURNS (', [NewLine]));
    for I := 0 to OutputFieldsCount - 1 do
    begin
      Stream.WriteString(NewLine + '   ');
      OutputFields[I].SaveToDDL(Stream, options);
      if I <> OutputFieldsCount - 1 then
        Stream.WriteString(',');
    end;
    Stream.WriteString(')');
  end;
end;

procedure TMetaProcedure.LoadFromQuery(QNames, QFields,
  QCharset, QArrayDim, QGrants: TUIBStatement; OIDs: TOIDProcedures; DefaultCharset: TCharacterSet);
begin
  FName := MetaQuote(Trim(QNames.Fields.AsString[0]));
  QNames.ReadBlob(1, FSource);
  QFields.Params.AsString[0] := SQLUnQuote(FName);

  if OIDProcFieldIn in OIDs then
  begin
    QFields.Params.AsSmallint[1] := 0; // in
    QFields.Open;
    while not QFields.Eof do
    begin
      TMetaProcInField.Create(Self, Ord(OIDProcFieldIn)).LoadFromQuery(QFields, QCharset, QArrayDim, DefaultCharset);
      QFields.Next;
    end;
  end;

  if OIDProcFieldOut in OIDs then
  begin
    QFields.Params.AsSmallint[1] := 1; // out
    QFields.Open;
    while not QFields.Eof do
    begin
      TMetaProcOutField.Create(Self, Ord(OIDProcFieldOut)).LoadFromQuery(QFields, QCharset, QArrayDim, DefaultCharset);
      QFields.Next;
    end;
  end;

  if OIDProcedureGrant in OIDs then
  begin
    { Load Procedure Grants }
    QGrants.Params.AsInteger[0] := 5;
    QGrants.Params.AsString[1] := SQLUnQuote(FName);
    QGrants.Open;
    LoadGrantsFromQuery(QGrants);
  end;
end;

procedure TMetaProcedure.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  ReadString(Stream, FSource);
end;

procedure TMetaProcedure.LoadGrantsFromQuery(QGrants: TUIBStatement);
var
  Current: TGrantee;
  Previous: TGrantee;
  G: TMetaProcedureGrant;
begin
  Current.UserType := -1;
  Previous.UserType := -1;

  while not QGrants.Eof do
  begin
    Current.User := Trim(QGrants.Fields.AsString[0]);
    Current.Grantor := Trim(QGrants.Fields.AsString[1]);
    Current.Option := QGrants.Fields.AsInteger[3] = 1;
    Current.UserType := QGRants.Fields.AsInteger[4];

    if (Previous.UserType <> -1) and (
         (Current.UserType = Previous.UserType) and
         (Current.User = Previous.User) and
         (Current.Grantor = Previous.Grantor) and
         (Current.Option = Previous.Option)
        ) then
    begin
      // C'est exactement le même droit que le précédent, fait au même
      // utilisateur avec la même option !
    end
    else
    begin
      if Previous.UserType <> -1 then
      begin
        G := FindGrant(Previous.Option);
        if not Assigned(G) then
          G := TMetaProcedureGrant.Create(Self,Ord(OIDProcedureGrant));

        G.LoadFromGrantee(Previous);

        Previous := Current;
      end
      else
      begin
        Previous := Current;
      end;
    end;

    QGrants.Next;
  end;

  if Current.UserType <> -1 then
  begin
    G := FindGrant(Current.Option);
    if not Assigned(G) then
      G := TMetaProcedureGrant.Create(Self,Ord(OIDProcedureGrant));

    G.LoadFromGrantee(Current);
  end;
end;

procedure TMetaProcedure.SaveToAlterDDL(Stream: TStringStream; options: TDDLOptions);
var
  S: String;
begin
  S := Trim(FSource);
  if SameText(Copy(S,1,4),'decl') then
    Insert('  ',S,1);

  InternalSaveToDDL(Stream, 'ALTER', options);
  Stream.WriteString(NewLine + 'AS ' + NewLine);
  Stream.WriteString(S);
  Stream.WriteString(';');
end;

procedure TMetaProcedure.SaveToCreateEmptyDDL(Stream: TStringStream; options: TDDLOptions);
begin
  InternalSaveToDDL(Stream, {$IFDEF FB15_UP}'CREATE OR ALTER'{$ELSE}'CREATE'{$ENDIF}, options);
  if OutputFieldsCount > 0 then
    Stream.WriteString(NewLine + 'AS' + NewLine + 'begin' + NewLine + '  suspend;' + NewLine + 'end;') else
    Stream.WriteString(NewLine + 'AS' + NewLine + 'begin' + NewLine + '  exit;' + NewLine + 'end;');
end;

procedure TMetaProcedure.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
var
  S: String;
begin
  S := Trim(FSource);
  if SameText(Copy(S,1,4),'decl') then
    Insert('  ',S,1);

  InternalSaveToDDL(Stream, 'CREATE', options);
  Stream.WriteString(NewLine + 'AS ');
  Stream.WriteString(S);
  Stream.WriteString(';');
end;

procedure TMetaProcedure.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  WriteString(Stream, FSource);
  inherited SaveToStream(Stream);
end;

class function TMetaProcedure.NodeType: TMetaNodeType;
begin
  Result := MetaProcedure;
end;

function TMetaProcedure.FindGrant(Option: Boolean): TMetaProcedureGrant;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to GrantsCount - 1 do
    if (Grants[I].Option = Option) then
    begin
      Result := Grants[I];
      Exit;
    end;
end;

function TMetaProcedure.GetAsAlterDDL: string;
var stream: TStringStream;
begin
  stream := TStringStream.Create('');
  try
    SaveToAlterDDL(stream, []);
    result := stream.DataString;
  finally
    stream.Free;
  end;
end;

function TMetaProcedure.GetAsAlterToEmptyDDL: string;
var stream: TStringStream;
begin
  stream := TStringStream.Create('');
  try
    InternalSaveToDDL(stream, 'ALTER', []);
    if OutputFieldsCount > 0 then
      Stream.WriteString(NewLine + 'AS' + NewLine + 'begin' + NewLine + '  suspend;' + NewLine + 'end;')
    else
      Stream.WriteString(NewLine + 'AS' + NewLine + 'begin' + NewLine + '  exit;' + NewLine + 'end;');
    result := stream.DataString;
  finally
    stream.Free;
  end;
end;

function TMetaProcedure.GetAsCreateEmptyDDL: string;
var stream: TStringStream;
begin
  stream := TStringStream.Create('');
  try
    SaveToCreateEmptyDDL(stream, []);
    result := stream.DataString;
  finally
    stream.Free;
  end;
end;

{ TMetaException }

class function TMetaException.NodeClass: string;
begin
  Result := 'Exception';
end;

procedure TMetaException.LoadFromQuery(QName: TUIBStatement);
begin
  FName := MetaQuote(Trim(QName.Fields.AsString[0]));
  FMessage := QName.Fields.AsString[1];
  FNumber := QName.Fields.AsInteger[2];
end;

procedure TMetaException.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  ReadString(Stream, FMessage);
  Stream.Read(FNumber, SizeOf(FNumber))
end;

procedure TMetaException.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
begin
  Stream.WriteString(Format('CREATE EXCEPTION %s %s;', [Name, QuotedStr(FMessage)]));
end;

procedure TMetaException.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  WriteString(Stream, FMessage);
  Stream.Write(FNumber, SizeOf(FNumber));
  inherited SaveToStream(Stream);
end;

class function TMetaException.NodeType: TMetaNodeType;
begin
  Result := MetaException;
end;

{ TMetaUDF }

constructor TMetaUDF.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  inherited Create(AOwner, ClassIndex);
  AddClass(TMetaUDFField);
end;

class function TMetaUDF.NodeClass: string;
begin
  Result := 'UDF';
end;

procedure TMetaUDF.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  ReadString(Stream, FModule);
  ReadString(Stream, FEntry);
  Stream.Read(FReturn, SizeOf(FReturn));
end;

procedure TMetaUDF.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
var
  I, C: Integer;
begin
  Stream.WriteString(Format('DECLARE EXTERNAL FUNCTION %s', [Name]));
  C := 0;

  if FReturn = 0 then
  begin // return position
    for I := 0 to FieldsCount - 1 do
      if Fields[I].Position <> Return then
      begin
        if C > 0 then
          Stream.WriteString(',');
        Stream.WriteString(NewLine + '  ');
        Fields[I].SaveToDDL(Stream, options);
        Inc(C);
      end;
    for I := 0 to FieldsCount - 1 do
      if Fields[I].Position = Return then
      begin
        Stream.WriteString(NewLine + '  RETURNS ');
        Fields[I].SaveToDDL(Stream, options);
        Break;
      end;
  end
  else
  begin
    for I := 0 to FieldsCount - 1 do
    begin
      if C > 0 then
        Stream.WriteString(',');
      Stream.WriteString(NewLine + '  ');
      Fields[I].SaveToDDL(Stream, options);
      Inc(C);
    end;
    Stream.WriteString(Format('%s  RETURNS PARAMETER %d', [NewLine, Freturn]));
  end;

  Stream.WriteString(Format('%s  ENTRY_POINT ''%s'' MODULE_NAME ''%s'';',
    [NewLine, FEntry, FModule]));
end;

procedure TMetaUDF.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  WriteString(Stream, FModule);
  WriteString(Stream, FEntry);
  Stream.Write(FReturn, SizeOf(FReturn));
  inherited SaveToStream(Stream);
end;

procedure TMetaUDF.LoadFromQuery(QNames, QFields, QCharset, QArrayDim: TUIBStatement; OIDs: TOIDUDFs; DefaultCharset: TCharacterSet);
begin
  FName := Trim(QNames.Fields.AsString[0]);
  FModule := QNames.Fields.AsString[1];
  FEntry := Trim(QNames.Fields.AsString[2]);
  FReturn := QNames.Fields.AsSmallint[3];

  if OIDUDFField in OIDs then
  begin
    QFields.Params.AsString[0] := SQLUnQuote(FName);
    QFields.Open;
    while not QFields.Eof do
    begin
      TMetaUDFField.Create(Self, Ord(OIDUDFField)).LoadFromQuery(QFields, QCharset, QArrayDim, DefaultCharset);
      QFields.Next;
    end;
  end;
end;

function TMetaUDF.GetFields(const Index: Integer): TMetaUDFField;
begin
  Result := TMetaUDFField(GetItems(Ord(OIDUDFField), Index))
end;

function TMetaUDF.GetFieldsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDUDFField)].Childs.Count;
end;

class function TMetaUDF.NodeType: TMetaNodeType;
begin
  Result := MetaUDF;
end;

{ TMetaTableField }

function TMetaTableField.GetArrayBounds(const index: Integer): TArrayBound;
begin
  result := FArrayBounds[index];
end;

function TMetaTableField.GetArrayBoundsCount: Integer;
begin
  result := System.Length(FArrayBounds);
end;

function TMetaTableField.GetDomain: TMetaDomain;
begin
  if FDomain >= 0 then
    Result := TMetaDatabase(FOwner.FOwner).Domains[FDomain]
  else
    Result := nil;
end;

function TMetaTableField.GetShortFieldType: string;
var i: integer;
begin
  result := inherited GetShortFieldType;
  if ArrayBoundsCount > 0 then
  begin
    result := result + format(' [%d:%d', [FArrayBounds[0].LowerBound, FArrayBounds[0].HigherBound]);
    for i := 1 to ArrayBoundsCount - 1 do
      result := result + format(',%d:%d', [FArrayBounds[i].LowerBound, FArrayBounds[i].HigherBound]);
    result := result+ ']';
  end;
end;

procedure TMetaTableField.LoadFromQuery(Q, C, A: TUIBStatement; DefaultCharset: TCharacterSet);
var i: Integer;
begin
  inherited LoadFromQuery(Q, C, A, DefaultCharset);
  FNotNull := (Q.Fields.AsSmallint[12] = 1);

  FDomain := -1;
  if not (Self is TMetaDomain) then
  begin
    if OIDDomain in TMetaDataBase(FOwner.FOwner).FOIDDatabases then
      if not (Q.Fields.IsNull[15] or (Copy(Q.Fields.AsString[15], 1, 4) = 'RDB$')) then
        FDomain :=
          TMetaDataBase(FOwner.FOwner).FindDomainIndex(MetaQuote(Trim(Q.Fields.AsString[15])));
    Q.ReadBlob(16, FComputedSource);
  end;

  if (FDomain < 0) or (Domain.ValidationSource = '') then
    Q.ReadBlob(13, FValidationSource);

  // Array
  if Q.Fields.AsSmallint[14] > 0 then
  begin
    SetLength(FArrayBounds, Q.Fields.AsSmallint[14]);
    if (Self is TMetaDomain) then
      A.Params.AsString[0] := FName else
      A.Params.AsString[0] := Q.Fields.AsString[15];
    A.Open;
    i := 0;
    while not A.Eof do
    begin
      FArrayBounds[i].LowerBound := A.Fields.AsInteger[0];
      FArrayBounds[i].HigherBound := A.Fields.AsInteger[1];
      inc(i);
      A.Next;
    end;
  end;
end;

procedure TMetaTableField.LoadFromStream(Stream: TStream);
var i: Integer;
begin
  inherited LoadFromStream(Stream);
  Stream.Read(FNotNull, SizeOf(FNotNull));
  Stream.Read(FDomain, SizeOf(FDomain));
  ReadString(Stream, FValidationSource);
  ReadString(Stream, FComputedSource);
  Stream.Read(i, Sizeof(i));
  SetLength(FArrayBounds, i);
  for i := 0 to i - 1 do
    Stream.Read(FArrayBounds[i], SizeOf(TArrayBound));
end;

class function TMetaTableField.NodeType: TMetaNodeType;
begin
  Result := MetaTableField;
end;

procedure TMetaTableField.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
var i: Integer;
begin
  if FDomain >= 0 then
    Stream.WriteString(Domain.Name) else
    if FComputedSource <> '' then
      Stream.WriteString('COMPUTED BY ' + FComputedSource) else
      begin
        case FFieldType of
          uftNumeric:
            if FSubType = 2 then
              Stream.WriteString(Format('DECIMAL(%d,%d)', [FPrecision, FScale])) else
              Stream.WriteString(Format('NUMERIC(%d,%d)', [FPrecision, FScale]));
          uftChar..uftCstring:
            begin
              Stream.WriteString(Format('%s(%d)',
                [FieldTypes[FFieldType], FLength div FBytesPerCharacter]));
            end;
          uftBlob:
            Stream.WriteString(Format('%s SUB_TYPE %d SEGMENT SIZE %d',
              [FieldTypes[FFieldType], FSubType, FSegmentLength]));
        else
          Stream.WriteString(Format('%s', [FieldTypes[FFieldType]]));
        end;

        if ArrayBoundsCount > 0 then
        begin
          Stream.WriteString(format(' [%d:%d', [FArrayBounds[0].LowerBound, FArrayBounds[0].HigherBound]));
          for i := 1 to ArrayBoundsCount - 1 do
            Stream.WriteString(format(',%d:%d', [FArrayBounds[i].LowerBound, FArrayBounds[i].HigherBound]));
          Stream.WriteString(']');
        end;

        if (FFieldType in [uftChar..uftCstring]) and (FCharSet <> '') then
          Stream.WriteString(' CHARACTER SET ' + FCharSet);
      end;

  if FDefaultValue <> '' then
    Stream.WriteString(' DEFAULT ' + FDefaultValue);
  if FNotNull then
    Stream.WriteString(' NOT NULL');
  if (FDomain < 0) and (FComputedSource = '') and
     (FFieldType in [uftChar..uftCstring]) and (FCollation <> '') then
    Stream.WriteString(' COLLATE ' + FCollation);
  if (FValidationSource <> '') then
    Stream.WriteString(' ' + FValidationSource);
end;

procedure TMetaTableField.SaveToStream(Stream: TStream);
var i: integer;
begin
  inherited SaveToStream(Stream);
  Stream.Write(FNotNull, SizeOf(FNotNull));
  Stream.Write(FDomain, SizeOf(FDomain));
  WriteString(Stream, FValidationSource);
  WriteString(Stream, FComputedSource);
  i := ArrayBoundsCount;
  Stream.Write(i, Sizeof(i));
  for i := 0 to i - 1 do
    Stream.Write(FArrayBounds[i], SizeOf(TArrayBound));
end;

{ TMetaProcInField }

class function TMetaProcInField.NodeClass: string;
begin
  Result := 'Input parameter';
end;

class function TMetaProcInField.NodeType: TMetaNodeType;
begin
  Result := MetaProcInField;
end;

{ TMetaProcOutField }

class function TMetaProcOutField.NodeClass: string;
begin
  Result := 'Output parameter';
end;

class function TMetaProcOutField.NodeType: TMetaNodeType;
begin
  Result := MetaProcOutField;
end;

{ TMetaField }

procedure TMetaField.LoadFromQuery(Q, C, A: TUIBStatement; DefaultCharset: TCharacterSet);
begin
  inherited LoadFromQuery(Q, C, A, DefaultCharset);
  FName := MetaQuote(Trim(Q.Fields.AsString[9]));
  FSegmentLength := Q.Fields.AsSmallint[10];
end;

class function TMetaField.NodeType: TMetaNodeType;
begin
  Result := MetaField;
end;

procedure TMetaField.SaveToDDL(Stream: TStringStream; options: TDDLOptions);
begin
  Stream.WriteString(Name + ' ');
  inherited SaveToDDL(Stream, options);
end;

{ TMetaUDFField }

procedure TMetaUDFField.LoadFromQuery(QField, QCharset, QArrayDim: TUIBStatement; DefaultCharset: TCharacterSet);
begin
  inherited LoadFromQuery(QField, QCharset, QArrayDim, DefaultCharset);
  FPosition := QField.Fields.AsSmallint[9];
  FMechanism := QField.Fields.AsSmallint[10];
  FName := 'Field ' + IntToStr(FPosition);
end;

procedure TMetaUDFField.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  Stream.Read(FPosition, SizeOf(FPosition));
  Stream.Read(FMechanism, SizeOf(FMechanism));
end;

class function TMetaUDFField.NodeType: TMetaNodeType;
begin
  Result := MetaUDFField;
end;

procedure TMetaUDFField.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
begin
  if FFieldType = uftBlob then
    Stream.WriteString('BLOB')
  else
    inherited SaveToDDLNode(Stream, options);
  case FMechanism of
    -1: Stream.WriteString(' FREE_IT');
     0: Stream.WriteString(' BY VALUE');
     1: ; // BY REFERENCE = default
     2: Stream.WriteString(' BY DESCRIPTOR');
  end;
end;

procedure TMetaUDFField.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(FPosition, SizeOf(FPosition));
  Stream.Write(FMechanism, SizeOf(FMechanism));
end;

{ TMetaRole }

constructor TMetaRole.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  inherited Create(AOwner, ClassIndex);
  AddClass(TMetaRoleGrant);
end;

function TMetaRole.FindGrant(Option: Boolean): TMetaRoleGrant;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to GrantsCount - 1 do
    if (Grants[I].Option = Option) then
    begin
      Result := Grants[I];
      Exit;
    end;
end;

function TMetaRole.GetRoleGrants(const Index: Integer): TMetaRoleGrant;
begin
  Result := TMetaRoleGrant(GetItems(Ord(OIDRoleGrant),Index));
end;

function TMetaRole.GetRoleGrantsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDRoleGrant)].Childs.Count;
end;

procedure TMetaRole.LoadFromQuery(QName, QGrants: TUIBStatement; OIDs: TOIDRoles);
begin
  FName := MetaQuote(Trim(QName.Fields.AsString[0]));
  FOwner := QName.Fields.AsString[1];

  if OIDRoleGrant in OIDs then
  begin
    { Load Role Grants }
    QGrants.Params.AsInteger[0] := 13;
    QGrants.Params.AsString[1] := SQLUnQuote(FName);
    QGrants.Open;
    LoadGrantsFromQuery(QGrants);
  end;
end;

procedure TMetaRole.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  ReadString(Stream, FOwner);
end;

procedure TMetaRole.LoadGrantsFromQuery(QGrants: TUIBStatement);
var
  Current: TGrantee;
  Previous: TGrantee;
  G: TMetaRoleGrant;
begin
  Current.UserType := -1;
  Previous.UserType := -1;

  while not QGrants.Eof do
  begin
    Current.User := Trim(QGrants.Fields.AsString[0]);
    Current.Grantor := Trim(QGrants.Fields.AsString[1]);
    Current.Option := QGrants.Fields.AsInteger[3] = 2;
    Current.UserType := QGRants.Fields.AsInteger[4];

    if (Previous.UserType <> -1) and (
         (Current.UserType = Previous.UserType) and
         (Current.User = Previous.User) and
         (Current.Grantor = Previous.Grantor) and
         (Current.Option = Previous.Option)
        ) then
    begin
      // C'est exactement le même droit que le précédent, fait au même
      // utilisateur avec la même option !
    end
    else
    begin
      if Previous.UserType <> -1 then
      begin
        G := FindGrant(Previous.Option);
        if not Assigned(G) then
          G := TMetaRoleGrant.Create(Self,Ord(OIDRoleGrant));

        G.LoadFromGrantee(Previous);

        Previous := Current;
      end
      else
      begin
        Previous := Current;
      end;
    end;

    QGrants.Next;
  end;

  if Current.UserType <> -1 then
  begin
    G := FindGrant(Current.Option);
    if not Assigned(G) then
      G := TMetaRoleGrant.Create(Self,Ord(OIDRoleGrant));

    G.LoadFromGrantee(Current);
  end;
end;

class function TMetaRole.NodeClass: string;
begin
  Result := 'Role';
end;

class function TMetaRole.NodeType: TMetaNodeType;
begin
  Result := MetaRole;
end;

procedure TMetaRole.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
begin
  Stream.WriteString(Format('CREATE ROLE %s; /* By user %s */', [Name, Trim(FOwner)]));
end;

procedure TMetaRole.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  WriteString(Stream, FOwner);
  inherited SaveToStream(Stream);
end;

{ TMetaGrant }

procedure TMetaGrant.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream,FName);
  Stream.Read(FOption,SizeOf(Boolean));
end;

class function TMetaGrant.NodeClass: string;
begin
  Result := 'Grant';
end;

class function TMetaGrant.NodeType: TMetaNodeType;
begin
  Result := MetaGrant;
end;

procedure TMetaGrant.SaveGranteesToDDLNode(Stream: TStringStream;
  OptionKeyWord: String; options: TDDLOptions);
var
  I, C: Integer;
  Grantees: Integer;
begin
  Grantees := 0;
  for I := 0 to FNodeItemsCount - 1 do
  begin
    for C := 0 to FNodeItems[I].Childs.Count - 1 do
    begin
      if Grantees > 0 then
        Stream.WriteString(', ');
      with TMetaGrantee(FNodeItems[I].Childs.Items[C]) do
        SaveToDDLNode(Stream, options);
      Inc(Grantees);
    end;
  end;

  if (Grantees > 0) and FOption then
    Stream.WriteString(' WITH ' + OptionKeyWord + ' OPTION');
  Stream.WriteString(';');
end;

procedure TMetaGrant.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
begin
  Stream.WriteString('GRANT ');
end;

procedure TMetaGrant.SaveToStream(Stream: TStream);
begin
  WriteString(Stream,FName);
  Stream.WriteBuffer(FOption,SizeOf(Boolean));
  inherited SaveToStream(Stream);
end;

{ TMetaRoleGrant }

constructor TMetaRoleGrant.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  inherited;
  AddClass(TMetaUserGrantee);
  FName := AOwner.FName;
end;

function TMetaRoleGrant.GetName: String;
begin
  Result := FName;
  if FOption then
    Result := Result + ' [ADMIN]';
end;

procedure TMetaRoleGrant.LoadFromGrantee(G: TGrantee);
begin
  TMetaUserGrantee.Create(Self,0).LoadFromGrantee(G);
  FOption := G.Option;  
end;

class function TMetaRoleGrant.NodeClass: string;
begin
  Result := 'Role Grant';
end;

class function TMetaRoleGrant.NodeType: TMetaNodeType;
begin
  Result := MetaRoleGrant;
end;

procedure TMetaRoleGrant.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
begin
  inherited SaveToDDLNode(Stream, options);
  Stream.WriteString('"' + FName + '" TO ');
  inherited SaveGranteesToDDLNode(Stream, 'ADMIN', options);
end;

{ TMetaObjectGrant }

constructor TMetaObjectGrant.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  inherited Create(AOwner, ClassIndex);
  AddClass(TMetaUserGrantee);
  AddClass(TMetaRoleGrantee);
  AddClass(TMetaProcedureGrantee);
  AddClass(TMetaTriggerGrantee);
  AddClass(TMetaViewGrantee);
  FName := AOwner.FName;
end;

procedure TMetaObjectGrant.LoadFromGrantee(G: TGrantee);
begin
  // Object/User Types used in RDB$USER_PRIVILEGES
  // Useful types for privileges management are those with the (*)

  // select RDB$TYPE, RDB$TYPE_NAME from rdb$types
  // where RDB$FIELD_NAME='RDB$OBJECT_TYPE'
  // order by RDB$TYPE

  //  0 => RELATION
  //  1 => VIEW                 (*)
  //  2 => TRIGGER              (*)
  //  3 => COMPUTED_FIELD
  //  4 => VALIDATION
  //  5 => PROCEDURE            (*)
  //  6 => EXPRESSION_INDEX
  //  7 => EXCEPTION
  //  8 => USER                 (*)
  //  9 => FIELD
  // 10 => INDEX
  // 11 => DEPENDENT_COUNT
  // 12 => USER_GROUP
  // 13 => ROLE                 (*)
  // 14 => GENERATOR
  // 15 => UDF
  // 16 => BLOB_FILTER

  case G.UserType of
  1  : TMetaViewGrantee.Create(Self,4).LoadFromGrantee(G);
  2  : TMetaTriggerGrantee.Create(Self,3).LoadFromGrantee(G);
  5  : TMetaProcedureGrantee.Create(Self,2).LoadFromGrantee(G);
  8  : TMetaUserGrantee.Create(Self,0).LoadFromGrantee(G);
  13 : TMetaRoleGrantee.Create(Self,1).LoadFromGrantee(G);
  else
    raise EUIBError.CreateFmt('Unsupported GRANT USER_TYPE %d',[G.UserType]);
  end;

  FOption := G.Option;  
end;

{ TMetaProcedureGrant }

function TMetaProcedureGrant.GetName: String;
begin
  Result := FName + ' : EXECUTE';
  if FOption then
    Result := Result + ' [GRANT]';
end;

class function TMetaProcedureGrant.NodeClass: string;
begin
  Result := 'Procedure Grant';
end;

class function TMetaProcedureGrant.NodeType: TMetaNodeType;
begin
  Result := MetaProcedureGrant;
end;

procedure TMetaProcedureGrant.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
begin
  inherited SaveToDDLNode(Stream, options);
  Stream.WriteString('EXECUTE ON PROCEDURE ' + MetaQuote(FName) + ' TO ');
  inherited SaveGranteesToDDLNode(Stream, 'GRANT', options);
end;

{ TMetaTableGrant }

function TMetaTableGrant.GetName: String;
begin
  Result := FName + ' : ';
  if FPrivileges = ALLTablePrivileges then
    Result := Result + 'ALL PRIVILEGES'
  else
  begin
    if tpSelect in FPrivileges then
      Result := Result + 'SELECT,';
    if tpInsert in FPrivileges then
      Result := Result + 'INSERT,';
    if tpUpdate in FPrivileges then
      Result := Result + 'UPDATE,';
    if tpDelete in FPrivileges then
      Result := Result + 'DELETE,';
    if tpReference in FPrivileges then
      Result := Result + 'REFERENCES,';
  end;
  Delete(Result,Length(Result),1);
  if FOption then
    Result := Result + ' [GRANT]';
end;

procedure TMetaTableGrant.LoadFromStream(Stream: TStream);
begin
  Stream.ReadBuffer(FPrivileges,SizeOf(TTablePrivileges));
  inherited LoadFromStream(Stream);
end;

class function TMetaTableGrant.NodeClass: string;
begin
  Result := 'Table/View Grant';
end;

class function TMetaTableGrant.NodeType: TMetaNodeType;
begin
  Result := MetaTableGrant;
end;

procedure TMetaTableGrant.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
var
  Grants: String;
begin
  inherited SaveToDDLNode(Stream, options);
  if FPrivileges = ALLTablePrivileges then
    Grants := 'ALL PRIVILEGES'
  else
  begin
    Grants := '';
    if tpSelect in FPrivileges then
      Grants := Grants + 'SELECT,';
    if tpInsert in FPrivileges then
      Grants := Grants + 'INSERT,';
    if tpUpdate in FPrivileges then
      Grants := Grants + 'UPDATE,';
    if tpDelete in FPrivileges then
      Grants := Grants + 'DELETE,';
    if tpReference in FPrivileges then
      Grants := Grants + 'REFERENCES,';
    Delete(Grants,Length(Grants),1);
  end;
  Stream.WriteString(Grants + ' ON ' + FName + ' TO ');
  inherited SaveGranteesToDDLNode(Stream, 'GRANT', options);
end;

procedure TMetaTableGrant.SaveToStream(Stream: TStream);
begin
  Stream.WriteBuffer(FPrivileges, SizeOf(TTablePrivileges));
  inherited SaveToStream(Stream);
end;

{ TMetaFieldGrant }

procedure TMetaFieldGrant.AssignFields(F: TStringList);
begin
  FFields.Assign(F);
end;

constructor TMetaFieldGrant.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  inherited Create(AOwner, ClassIndex);
  FFields := TStringList.Create;
end;

destructor TMetaFieldGrant.Destroy;
begin
  FFields.Free;
  inherited;
end;

function TMetaFieldGrant.GetFields(const Index: Integer): String;
begin
  Result := FFields[Index];
end;

function TMetaFieldGrant.GetFieldsCount: Integer;
begin
  Result := FFields.Count;
end;

function TMetaFieldGrant.GetName: String;
var
  I: Integer;
begin
  Result := FName + ' : ';
  if fpUpdate = FPrivilege then
    Result := Result + 'UPDATE(';

  if fpReference = FPrivilege then
    Result := Result + 'REFERENCE(';

  for I := 0 to FFields.Count - 1 do
    Result := Result + FFields[i] + ', ';

  Result[Length(Result) - 1] := ')';

  if FOption then
    Result := Result + '[GRANT]';
end;

function TMetaFieldGrant.HasFields(F: TStringList): Boolean;
var
  I: Integer;
begin
  Result := false;
  if FFields.Count = F.Count then
  begin
    for I := 0 to F.Count - 1 do
      if FFields[I] <> F[I] then
        Break;
    Result := true;
  end;
end;

procedure TMetaFieldGrant.LoadFromStream(Stream: TStream);
var
  F: Integer;
  Field: String;
begin
  Stream.ReadBuffer(FPrivilege,SizeOf(TFieldPrivilege));
  Stream.ReadBuffer(F,SizeOf(Integer));
  FFields.Clear;
  while F > 0 do
  begin
    ReadString(Stream,Field);
    FFields.Append(Field);
    Dec(F);
  end;
  inherited LoadFromStream(Stream);
end;

class function TMetaFieldGrant.NodeClass: string;
begin
  Result := 'Field Grant';
end;

class function TMetaFieldGrant.NodeType: TMetaNodeType;
begin
  Result := MetaFieldGrant;
end;

procedure TMetaFieldGrant.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
var
  I: Integer;
  Grants: String;
begin
  inherited SaveToDDLNode(Stream, options);
  Grants := '';
  if fpUpdate = FPrivilege then
    Grants := Grants + 'UPDATE('
  else if fpReference = FPrivilege then
    Grants := Grants + 'REFERENCE(';
  for I := 0 to FFields.Count - 1 do
    Grants := Grants + FFields[i] + ', ';
  Grants[Length(Grants) - 1] := ')';
  Stream.WriteString(Grants + 'ON ' + Name + ' TO ');
  inherited SaveGranteesToDDLNode(Stream, 'GRANT', options);
end;

procedure TMetaFieldGrant.SaveToStream(Stream: TStream);
var
  F: Integer;
  Field: String;
begin
  Stream.WriteBuffer(FPrivilege,SizeOf(TFieldPrivilege));
  F := FFields.Count;
  Stream.WriteBuffer(F, SizeOf(Integer));
  for F := 0 to FFields.Count - 1 do
  begin
    Field := FFields[F];
    WriteString(Stream,Field);
  end;
  inherited SaveToStream(Stream);
end;

{ TMetaGrantee }

procedure TMetaGrantee.LoadFromGrantee(G: TGrantee);
begin
  FName := G.User;
  FGrantor := G.Grantor;
end;

procedure TMetaGrantee.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream,FName);
  ReadString(Stream,FGrantor);
end;

class function TMetaGrantee.NodeClass: string;
begin
  Result := 'Grantee';
end;

class function TMetaGrantee.NodeType: TMetaNodeType;
begin
  Result := MetaGrantee;
end;

procedure TMetaGrantee.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
begin
  inherited SaveToDDLNode(Stream, options);
  Stream.WriteString(Name);
(*
  { Add some comments for debugging purposes }
  Stream.WriteString(' /* Grantor : ' + FGrantor + ' */');
*)
end;

procedure TMetaGrantee.SaveToStream(Stream: TStream);
begin
  WriteString(Stream,FName);
  WriteString(Stream,FGrantor);
  inherited SaveToStream(Stream);
end;

{ TMetaUserGrantee }

class function TMetaUserGrantee.NodeClass: string;
begin
  Result := 'User Grantee';
end;

class function TMetaUserGrantee.NodeType: TMetaNodeType;
begin
  Result := MetaUserGrantee;
end;

{ TMetaRoleGrantee }

class function TMetaRoleGrantee.NodeClass: string;
begin
  Result := 'Role Grantee';
end;

class function TMetaRoleGrantee.NodeType: TMetaNodeType;
begin
  Result := MetaRoleGrantee;
end;

{ TMetaProcedureGrantee }

class function TMetaProcedureGrantee.NodeClass: string;
begin
  Result := 'Procedure Grantee';
end;

class function TMetaProcedureGrantee.NodeType: TMetaNodeType;
begin
  Result := MetaProcedureGrantee;
end;

procedure TMetaProcedureGrantee.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
begin
  Stream.WriteString('PROCEDURE ');
  inherited SaveToDDLNode(Stream, options);
end;

{ TMetaTriggerGrantee }

class function TMetaTriggerGrantee.NodeClass: string;
begin
  Result := 'Trigger Grantee';
end;

class function TMetaTriggerGrantee.NodeType: TMetaNodeType;
begin
  Result := MetaTriggerGrantee;
end;

procedure TMetaTriggerGrantee.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
begin
  Stream.WriteString('TRIGGER ');
  inherited SaveToDDLNode(Stream, options);
end;

{ TMetaViewGrantee }

class function TMetaViewGrantee.NodeClass: string;
begin
  Result := 'View Grantee';
end;

class function TMetaViewGrantee.NodeType: TMetaNodeType;
begin
  Result := MetaViewGrantee;
end;

procedure TMetaViewGrantee.SaveToDDLNode(Stream: TStringStream; options: TDDLOptions);
begin
  Stream.WriteString('VIEW ');
  inherited SaveToDDLNode(Stream, options);
end;

end.

