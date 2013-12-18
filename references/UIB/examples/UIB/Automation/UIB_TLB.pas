unit UIB_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 04/01/2004 10:06:19 from Type Library described below.

// ************************************************************************  //
// Type Lib: D:\uib\UIB\examples\uib\Automation\UIB.tlb (1)
// LIBID: {4D24084B-76DE-4197-A320-BBA5235EB35A}
// LCID: 0
// Helpfile: 
// HelpString: UIB Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\System32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  UIBMajorVersion = 1;
  UIBMinorVersion = 0;

  LIBID_UIB: TGUID = '{4D24084B-76DE-4197-A320-BBA5235EB35A}';

  IID_IDatabase: TGUID = '{F739B7D5-069E-473A-B0AA-3E49256E446F}';
  CLASS_Database: TGUID = '{A3304292-DD8A-4475-BD8F-9C82EA4E6E3A}';
  IID_ITransaction: TGUID = '{735A0EF8-1433-4C91-B4BB-05E3BFCC18B7}';
  CLASS_Transaction: TGUID = '{2DE61B04-F7FC-4804-BC24-629F75CD629E}';
  IID_IQuery: TGUID = '{82EFAD8B-4406-4455-84F1-A911D2C6C0D1}';
  CLASS_Query: TGUID = '{9043E8BC-ED4B-48C7-9018-A17515C46644}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IDatabase = interface;
  IDatabaseDisp = dispinterface;
  ITransaction = interface;
  ITransactionDisp = dispinterface;
  IQuery = interface;
  IQueryDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  Database = IDatabase;
  Transaction = ITransaction;
  Query = IQuery;


// *********************************************************************//
// Interface: IDatabase
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F739B7D5-069E-473A-B0AA-3E49256E446F}
// *********************************************************************//
  IDatabase = interface(IDispatch)
    ['{F739B7D5-069E-473A-B0AA-3E49256E446F}']
    function Get_DatabaseName: WideString; safecall;
    procedure Set_DatabaseName(const Value: WideString); safecall;
    function Get_Connected: WordBool; safecall;
    procedure Set_Connected(Value: WordBool); safecall;
    function Get_SQLDialect: Integer; safecall;
    procedure Set_SQLDialect(Value: Integer); safecall;
    function Get_CharacterSet: WideString; safecall;
    procedure Set_CharacterSet(const Value: WideString); safecall;
    function Get_UserName: WideString; safecall;
    procedure Set_UserName(const Value: WideString); safecall;
    function Get_PassWord: WideString; safecall;
    procedure Set_PassWord(const Value: WideString); safecall;
    property DatabaseName: WideString read Get_DatabaseName write Set_DatabaseName;
    property Connected: WordBool read Get_Connected write Set_Connected;
    property SQLDialect: Integer read Get_SQLDialect write Set_SQLDialect;
    property CharacterSet: WideString read Get_CharacterSet write Set_CharacterSet;
    property UserName: WideString read Get_UserName write Set_UserName;
    property PassWord: WideString read Get_PassWord write Set_PassWord;
  end;

// *********************************************************************//
// DispIntf:  IDatabaseDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F739B7D5-069E-473A-B0AA-3E49256E446F}
// *********************************************************************//
  IDatabaseDisp = dispinterface
    ['{F739B7D5-069E-473A-B0AA-3E49256E446F}']
    property DatabaseName: WideString dispid 201;
    property Connected: WordBool dispid 202;
    property SQLDialect: Integer dispid 203;
    property CharacterSet: WideString dispid 204;
    property UserName: WideString dispid 205;
    property PassWord: WideString dispid 206;
  end;

// *********************************************************************//
// Interface: ITransaction
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {735A0EF8-1433-4C91-B4BB-05E3BFCC18B7}
// *********************************************************************//
  ITransaction = interface(IDispatch)
    ['{735A0EF8-1433-4C91-B4BB-05E3BFCC18B7}']
    function Get_Database: IDatabase; safecall;
    procedure Set_Database(const Value: IDatabase); safecall;
    procedure Commit; safecall;
    procedure CommitRetaining; safecall;
    procedure RollBack; safecall;
    procedure RollBackRetaining; safecall;
    function Get_InTransaction: WordBool; safecall;
    property Database: IDatabase read Get_Database write Set_Database;
    property InTransaction: WordBool read Get_InTransaction;
  end;

// *********************************************************************//
// DispIntf:  ITransactionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {735A0EF8-1433-4C91-B4BB-05E3BFCC18B7}
// *********************************************************************//
  ITransactionDisp = dispinterface
    ['{735A0EF8-1433-4C91-B4BB-05E3BFCC18B7}']
    property Database: IDatabase dispid 201;
    procedure Commit; dispid 202;
    procedure CommitRetaining; dispid 203;
    procedure RollBack; dispid 204;
    procedure RollBackRetaining; dispid 205;
    property InTransaction: WordBool readonly dispid 206;
  end;

// *********************************************************************//
// Interface: IQuery
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {82EFAD8B-4406-4455-84F1-A911D2C6C0D1}
// *********************************************************************//
  IQuery = interface(IDispatch)
    ['{82EFAD8B-4406-4455-84F1-A911D2C6C0D1}']
    function Get_FetchBlobs: WordBool; safecall;
    procedure Set_FetchBlobs(Value: WordBool); safecall;
    function Get_CachedFetch: WordBool; safecall;
    procedure Set_CachedFetch(Value: WordBool); safecall;
    function Get_QuickScript: WordBool; safecall;
    procedure Set_QuickScript(Value: WordBool); safecall;
    function Get_EOF: WordBool; safecall;
    procedure Open; safecall;
    procedure ExecSQL; safecall;
    procedure Close(Mode: Integer); safecall;
    procedure Next; safecall;
    procedure FetchAll; safecall;
    procedure SQLAddLine(const Str: WideString); safecall;
    function Get_Transaction: ITransaction; safecall;
    procedure Set_Transaction(const Value: ITransaction); safecall;
    function Get_AsVariant(Index: Word): OleVariant; safecall;
    function Get_SQL: WideString; safecall;
    procedure Set_SQL(const Value: WideString); safecall;
    function Get_AsInteger(Index: Word): Integer; safecall;
    function Get_AsSmallint(Index: Word): Smallint; safecall;
    function Get_AsSingle(Index: Word): Single; safecall;
    function Get_AsDouble(Index: Word): Double; safecall;
    function Get_AsInt64(Index: Word): Int64; safecall;
    function Get_AsString(Index: Word): WideString; safecall;
    function Get_ByNameAsSmallint(const Name: WideString): Smallint; safecall;
    function Get_ByNameAsInteger(const Name: WideString): Integer; safecall;
    function Get_ByNameAsSingle(const Name: WideString): Single; safecall;
    function Get_ByNameAsDouble(const Name: WideString): Double; safecall;
    function Get_ByNameAsInt64(const Name: WideString): Int64; safecall;
    function Get_ByNameAsString(const Name: WideString): WideString; safecall;
    function Get_ByNameAsVariant(const Name: WideString): OleVariant; safecall;
    function Get_FieldCount: Integer; safecall;
    function Get_AliasName(Index: Word): WideString; safecall;
    function Get_AsBoolean(Index: Word): WordBool; safecall;
    function Get_ByNameAsBoolean(const Name: WideString): WordBool; safecall;
    function Get_AsDateTime(Index: Word): TDateTime; safecall;
    function Get_ByNameAsDateTime(const Name: WideString): TDateTime; safecall;
    property FetchBlobs: WordBool read Get_FetchBlobs write Set_FetchBlobs;
    property CachedFetch: WordBool read Get_CachedFetch write Set_CachedFetch;
    property QuickScript: WordBool read Get_QuickScript write Set_QuickScript;
    property EOF: WordBool read Get_EOF;
    property Transaction: ITransaction read Get_Transaction write Set_Transaction;
    property AsVariant[Index: Word]: OleVariant read Get_AsVariant;
    property SQL: WideString read Get_SQL write Set_SQL;
    property AsInteger[Index: Word]: Integer read Get_AsInteger;
    property AsSmallint[Index: Word]: Smallint read Get_AsSmallint;
    property AsSingle[Index: Word]: Single read Get_AsSingle;
    property AsDouble[Index: Word]: Double read Get_AsDouble;
    property AsInt64[Index: Word]: Int64 read Get_AsInt64;
    property AsString[Index: Word]: WideString read Get_AsString;
    property ByNameAsSmallint[const Name: WideString]: Smallint read Get_ByNameAsSmallint;
    property ByNameAsInteger[const Name: WideString]: Integer read Get_ByNameAsInteger;
    property ByNameAsSingle[const Name: WideString]: Single read Get_ByNameAsSingle;
    property ByNameAsDouble[const Name: WideString]: Double read Get_ByNameAsDouble;
    property ByNameAsInt64[const Name: WideString]: Int64 read Get_ByNameAsInt64;
    property ByNameAsString[const Name: WideString]: WideString read Get_ByNameAsString;
    property ByNameAsVariant[const Name: WideString]: OleVariant read Get_ByNameAsVariant;
    property FieldCount: Integer read Get_FieldCount;
    property AliasName[Index: Word]: WideString read Get_AliasName;
    property AsBoolean[Index: Word]: WordBool read Get_AsBoolean;
    property ByNameAsBoolean[const Name: WideString]: WordBool read Get_ByNameAsBoolean;
    property AsDateTime[Index: Word]: TDateTime read Get_AsDateTime;
    property ByNameAsDateTime[const Name: WideString]: TDateTime read Get_ByNameAsDateTime;
  end;

// *********************************************************************//
// DispIntf:  IQueryDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {82EFAD8B-4406-4455-84F1-A911D2C6C0D1}
// *********************************************************************//
  IQueryDisp = dispinterface
    ['{82EFAD8B-4406-4455-84F1-A911D2C6C0D1}']
    property FetchBlobs: WordBool dispid 202;
    property CachedFetch: WordBool dispid 203;
    property QuickScript: WordBool dispid 204;
    property EOF: WordBool readonly dispid 206;
    procedure Open; dispid 207;
    procedure ExecSQL; dispid 208;
    procedure Close(Mode: Integer); dispid 209;
    procedure Next; dispid 210;
    procedure FetchAll; dispid 211;
    procedure SQLAddLine(const Str: WideString); dispid 212;
    property Transaction: ITransaction dispid 213;
    property AsVariant[Index: {??Word}OleVariant]: OleVariant readonly dispid 201;
    property SQL: WideString dispid 205;
    property AsInteger[Index: {??Word}OleVariant]: Integer readonly dispid 214;
    property AsSmallint[Index: {??Word}OleVariant]: Smallint readonly dispid 215;
    property AsSingle[Index: {??Word}OleVariant]: Single readonly dispid 216;
    property AsDouble[Index: {??Word}OleVariant]: Double readonly dispid 217;
    property AsInt64[Index: {??Word}OleVariant]: {??Int64}OleVariant readonly dispid 218;
    property AsString[Index: {??Word}OleVariant]: WideString readonly dispid 219;
    property ByNameAsSmallint[const Name: WideString]: Smallint readonly dispid 220;
    property ByNameAsInteger[const Name: WideString]: Integer readonly dispid 221;
    property ByNameAsSingle[const Name: WideString]: Single readonly dispid 222;
    property ByNameAsDouble[const Name: WideString]: Double readonly dispid 223;
    property ByNameAsInt64[const Name: WideString]: {??Int64}OleVariant readonly dispid 224;
    property ByNameAsString[const Name: WideString]: WideString readonly dispid 225;
    property ByNameAsVariant[const Name: WideString]: OleVariant readonly dispid 226;
    property FieldCount: Integer readonly dispid 227;
    property AliasName[Index: {??Word}OleVariant]: WideString readonly dispid 228;
    property AsBoolean[Index: {??Word}OleVariant]: WordBool readonly dispid 229;
    property ByNameAsBoolean[const Name: WideString]: WordBool readonly dispid 230;
    property AsDateTime[Index: {??Word}OleVariant]: TDateTime readonly dispid 231;
    property ByNameAsDateTime[const Name: WideString]: TDateTime readonly dispid 232;
  end;

// *********************************************************************//
// The Class CoDatabase provides a Create and CreateRemote method to          
// create instances of the default interface IDatabase exposed by              
// the CoClass Database. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoDatabase = class
    class function Create: IDatabase;
    class function CreateRemote(const MachineName: string): IDatabase;
  end;

// *********************************************************************//
// The Class CoTransaction provides a Create and CreateRemote method to          
// create instances of the default interface ITransaction exposed by              
// the CoClass Transaction. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoTransaction = class
    class function Create: ITransaction;
    class function CreateRemote(const MachineName: string): ITransaction;
  end;

// *********************************************************************//
// The Class CoQuery provides a Create and CreateRemote method to          
// create instances of the default interface IQuery exposed by              
// the CoClass Query. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoQuery = class
    class function Create: IQuery;
    class function CreateRemote(const MachineName: string): IQuery;
  end;

implementation

uses ComObj;

class function CoDatabase.Create: IDatabase;
begin
  Result := CreateComObject(CLASS_Database) as IDatabase;
end;

class function CoDatabase.CreateRemote(const MachineName: string): IDatabase;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Database) as IDatabase;
end;

class function CoTransaction.Create: ITransaction;
begin
  Result := CreateComObject(CLASS_Transaction) as ITransaction;
end;

class function CoTransaction.CreateRemote(const MachineName: string): ITransaction;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Transaction) as ITransaction;
end;

class function CoQuery.Create: IQuery;
begin
  Result := CreateComObject(CLASS_Query) as IQuery;
end;

class function CoQuery.CreateRemote(const MachineName: string): IQuery;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Query) as IQuery;
end;

end.
