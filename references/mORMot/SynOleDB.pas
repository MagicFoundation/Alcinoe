/// fast OleDB direct access classes
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynOleDB;

{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Esteban Martin (EMartin)
  - Pavel Mashlyakovskii (mpv)

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Several implementation notes about Oracle and OleDB:
  - Oracle OleDB provider by Microsoft do not handle BLOBs. Period. :(
  - Oracle OleDB provider by Oracle will handle only 3/4 BLOBs. :(
    See https://stackoverflow.com/a/6640101
  - Oracle OleDB provider by Oracle or Microsoft could trigger some ORA-80040e4B
    error when accessing column data with very low dates value (like 0001-01-01)
  - in all cases, that's why we wrote the SynDBOracle unit, for direct OCI
    access - and it is from 2 to 10 times faster than OleDB, with no setup issue
  - or take a look at latest patches from Oracle support, and pray it's fixed ;)
    https://stackoverflow.com/a/6661058

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

{$ifdef MSWINDOWS} // compiles as void unit for non-Windows - allow Lazarus package

uses
  Windows,
  {$ifdef ISDELPHIXE2}System.Win.ComObj,{$else}ComObj,{$endif}
  ActiveX,
  SysUtils,
{$ifndef DELPHI5OROLDER}
  Variants,
{$endif}
  Classes,
  Contnrs,
  SynCommons,
  SynLog,
  SynTable,
  SynDB;


{ -------------- OleDB interfaces, constants and types
  (OleDB.pas is not provided e.g. in Delphi 7 Personal) }


const
  IID_IUnknown: TGUID = '{00000000-0000-0000-C000-000000000046}';
  IID_IAccessor: TGUID = '{0C733A8C-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IRowset: TGUID = '{0C733A7C-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IMultipleResults: TGUID = '{0C733A90-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IOpenRowset: TGUID = '{0C733A69-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IDataInitialize: TGUID = '{2206CCB1-19C1-11D1-89E0-00C04FD7A829}';
  IID_IDBInitialize: TGUID = '{0C733A8B-2A1C-11CE-ADE5-00AA0044773D}';
  IID_ICommandText: TGUID = '{0C733A27-2A1C-11CE-ADE5-00AA0044773D}';
  IID_ISSCommandWithParameters: TGUID = '{EEC30162-6087-467C-B995-7C523CE96561}';
  IID_ITransactionLocal: TGUID = '{0C733A5F-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IDBPromptInitialize: TGUID = '{2206CCB0-19C1-11D1-89E0-00C04FD7A829}';
  CLSID_DATALINKS: TGUID = '{2206CDB2-19C1-11D1-89E0-00C04FD7A829}';
  CLSID_MSDAINITIALIZE: TGUID = '{2206CDB0-19C1-11D1-89E0-00C04FD7A829}';
  CLSID_ROWSET_TVP: TGUID = '{C7EF28D5-7BEE-443F-86DA-E3984FCD4DF9}';
  DB_NULLGUID: TGuid = '{00000000-0000-0000-0000-000000000000}';
  DBGUID_DEFAULT: TGUID = '{C8B521FB-5CF3-11CE-ADE5-00AA0044773D}';
  DBSCHEMA_TABLES: TGUID = '{C8B52229-5CF3-11CE-ADE5-00AA0044773D}';
  DBSCHEMA_COLUMNS: TGUID = '{C8B52214-5CF3-11CE-ADE5-00AA0044773D}';
  DBSCHEMA_INDEXES: TGUID = '{C8B5221E-5CF3-11CE-ADE5-00AA0044773D}';
  DBSCHEMA_FOREIGN_KEYS: TGUID = '{C8B522C4-5CF3-11CE-ADE5-00AA0044773D}';

  DBPROPSET_SQLSERVERPARAMETER: TGUID = '{FEE09128-A67D-47EA-8D40-24A1D4737E8D}';
  // PropIds for DBPROPSET_SQLSERVERPARAMETER
  SSPROP_PARAM_XML_SCHEMACOLLECTION_CATALOGNAME = 24;
  SSPROP_PARAM_XML_SCHEMACOLLECTION_SCHEMANAME = 25;
  SSPROP_PARAM_XML_SCHEMACOLLECTIONNAME = 26;
  SSPROP_PARAM_UDT_CATALOGNAME = 27;
  SSPROP_PARAM_UDT_SCHEMANAME = 28;
  SSPROP_PARAM_UDT_NAME = 29;
  SSPROP_PARAM_TYPE_CATALOGNAME = 38;
  SSPROP_PARAM_TYPE_SCHEMANAME = 39;
  SSPROP_PARAM_TYPE_TYPENAME = 40;
  SSPROP_PARAM_TABLE_DEFAULT_COLUMNS = 41;
  SSPROP_PARAM_TABLE_COLUMN_SORT_ORDER = 42;

  DBTYPE_EMPTY = $00000000;
  DBTYPE_NULL = $00000001;
  DBTYPE_I2 = $00000002;
  DBTYPE_I4 = $00000003;
  DBTYPE_R4 = $00000004;
  DBTYPE_R8 = $00000005;
  DBTYPE_CY = $00000006;
  DBTYPE_DATE = $00000007;
  DBTYPE_BSTR = $00000008;
  DBTYPE_IDISPATCH = $00000009;
  DBTYPE_ERROR = $0000000A;
  DBTYPE_BOOL = $0000000B;
  DBTYPE_VARIANT = $0000000C;
  DBTYPE_IUNKNOWN = $0000000D;
  DBTYPE_DECIMAL = $0000000E;
  DBTYPE_UI1 = $00000011;
  DBTYPE_ARRAY = $00002000;
  DBTYPE_BYREF = $00004000;
  DBTYPE_I1 = $00000010;
  DBTYPE_UI2 = $00000012;
  DBTYPE_UI4 = $00000013;
  DBTYPE_I8 = $00000014;
  DBTYPE_UI8 = $00000015;
  DBTYPE_GUID = $00000048;
  DBTYPE_VECTOR = $00001000;
  DBTYPE_RESERVED = $00008000;
  DBTYPE_BYTES = $00000080;
  DBTYPE_STR = $00000081;
  DBTYPE_WSTR = $00000082;
  DBTYPE_NUMERIC = $00000083;
  DBTYPE_UDT = $00000084;
  DBTYPE_DBDATE = $00000085;
  DBTYPE_DBTIME = $00000086;
  DBTYPE_DBTIMESTAMP = $00000087;
  DBTYPE_FILETIME = $00000040;
  DBTYPE_DBFILETIME = $00000089;
  DBTYPE_PROPVARIANT = $0000008A;
  DBTYPE_VARNUMERIC = $0000008B;
  DBTYPE_TABLE = $0000008F; // introduced in SQL 2008

  DBPARAMIO_NOTPARAM = $00000000;
  DBPARAMIO_INPUT = $00000001;
  DBPARAMIO_OUTPUT = $00000002;

  DBPARAMFLAGS_ISINPUT    = $00000001;
  DBPARAMFLAGS_ISOUTPUT   = $00000002;
  DBPARAMFLAGS_ISSIGNED   = $00000010;
  DBPARAMFLAGS_ISNULLABLE = $00000040;
  DBPARAMFLAGS_ISLONG     = $00000080;

  DBPART_VALUE = $00000001;
  DBPART_LENGTH = $00000002;
  DBPART_STATUS = $00000004;

  DBMEMOWNER_CLIENTOWNED = $00000000;
  DBMEMOWNER_PROVIDEROWNED = $00000001;

  DBACCESSOR_ROWDATA = $00000002;
  DBACCESSOR_PARAMETERDATA = $00000004;
  DBACCESSOR_OPTIMIZED = $00000008;

  DB_E_CANCELED = HResult($80040E4E);
  DB_E_NOTSUPPORTED = HResult($80040E53);
  DBCOLUMNFLAGS_MAYBENULL = $00000040;
  ISOLATIONLEVEL_READCOMMITTED = $00001000;
  DBPROMPTOPTIONS_PROPERTYSHEET = $2;
  DB_NULL_HCHAPTER = $00;
  DB_S_ENDOFROWSET = $00040EC6;
  XACTTC_SYNC = $00000002;

  MAXBOUND = 65535; { High bound for arrays }

  DBKIND_GUID_NAME     = 0;
  DBKIND_GUID_PROPID   = ( DBKIND_GUID_NAME + 1 );
  DBKIND_NAME          = ( DBKIND_GUID_PROPID + 1 );
  DBKIND_PGUID_NAME    = ( DBKIND_NAME + 1 );
  DBKIND_PGUID_PROPID  = ( DBKIND_PGUID_NAME + 1 );
  DBKIND_PROPID        = ( DBKIND_PGUID_PROPID + 1 );
  DBKIND_GUID          = ( DBKIND_PROPID + 1 );

type
  /// indicates whether the data value or some other value, such as a NULL,
  // is to be used as the value of the column or parameter
  // - see http://msdn.microsoft.com/en-us/library/ms722617
  // and http://msdn.microsoft.com/en-us/library/windows/desktop/ms716934
  TOleDBStatus = (
    stOK, stBadAccessor, stCanNotConvertValue, stIsNull, stTruncated,
    stSignMismatch, stDataoverFlow, stCanNotCreateValue, stUnavailable,
    stPermissionDenied, stIntegrityViolation, stSchemaViolation, stBadStatus,
    stDefault, stCellEmpty, stIgnoreColumn, stDoesNotExist, stInvalidURL,
    stResourceLocked, stResoruceExists, stCannotComplete, stVolumeNotFound,
    stOutOfSpace, stCannotDeleteSource, stAlreadyExists, stCanceled,
    stNotCollection, stRowSetColumn);

  /// binding status of a given column
  // - see http://msdn.microsoft.com/en-us/library/windows/desktop/ms720969
  // and http://msdn.microsoft.com/en-us/library/windows/desktop/ms716934
  TOleDBBindStatus = (
    bsOK, bsBadOrdinal, bsUnsupportedConversion, bsBadBindInfo,
    bsBadStorageFlags, bsNoInterface, bsMultipleStorage);

  PIUnknown = ^IUnknown;
  HACCESSOR = PtrUInt;
  HACCESSORDynArray = array of HACCESSOR;
  HCHAPTER = PtrUInt;
  HROW = PtrUInt;
  PHROW = ^HROW;

  DBPART = UINT;
  DBMEMOWNER = UINT;
  DBPARAMIO = UINT;
  DBPROPSTATUS = UINT;
  DBPROPID = UINT;
  DBPROPOPTIONS = UINT;
  DBCOLUMNFLAGS = UINT;
  DBKIND = UINT;
  DBSTATUS = DWORD;
  DBPARAMFLAGS = DWORD;
  DBTYPE = Word;
  DBRESULTFLAG = UINT;

  DBLENGTH = PtrUInt;
  DB_UPARAMS = PtrUInt;
  DBORDINAL = PtrUInt;

  PBoid = ^TBoid;
{$ifdef CPU64}
  {$A8} // un-packed records
{$else}
  {$A-} // packed records
{$endif}
  TBoid = record
    rgb_: array[0..15] of Byte;
  end;
  TXactOpt = record
    ulTimeout: UINT;
    szDescription: array[0..39] of Shortint;
  end;
  TXactTransInfo = record
    uow: PBoid;
    isoLevel: Integer;
    isoFlags: UINT;
    grfTCSupported: UINT;
    grfRMSupported: UINT;
    grfTCSupportedRetaining: UINT;
    grfRMSupportedRetaining: UINT;
  end;
  PErrorInfo = ^TErrorInfo;
  TErrorInfo = record
    hrError: HResult;
    dwMinor: UINT;
    clsid: TGUID;
    iid: TGUID;
    dispid: Integer;
  end;
  PDBParams = ^TDBParams;
  TDBParams = record
    pData: Pointer;
    cParamSets: PtrUInt;
    HACCESSOR: HACCESSOR;
  end;
  PDBObject = ^TDBObject;
  TDBObject = record
    dwFlags: UINT;
    iid: TGUID;
  end;
  PDBBindExt = ^TDBBindExt;
  TDBBindExt = record
    pExtension: PByte;
    ulExtension: PtrUInt;
  end;
  PDBBinding = ^TDBBinding;
  TDBBinding = record
    iOrdinal: DBORDINAL;
    obValue: PtrUInt;
    obLength: PtrUInt;
    obStatus: PtrUInt;
    pTypeInfo: ITypeInfo;
    pObject: PDBObject;
    pBindExt: PDBBindExt;
    dwPart: DBPART;
    dwMemOwner: DBMEMOWNER;
    eParamIO: DBPARAMIO;
    cbMaxLen: PtrUInt;
    dwFlags: UINT;
    wType: DBTYPE;
    bPrecision: Byte;
    bScale: Byte;
  end;
  PDBBindingArray = ^TDBBindingArray;
  TDBBindingArray = array[0..MAXBOUND] of TDBBinding;
  TDBBindingDynArray = array of TDBBinding;
  DBIDGUID = record
    case Integer of
      0: (guid: TGUID);
      1: (pguid: ^TGUID);
  end;
  DBIDNAME = record
    case Integer of
      0: (pwszName: PWideChar);
      1: (ulPropid: UINT);
  end;
  PDBID = ^DBID;
  DBID = record
    uGuid: DBIDGUID;
    eKind: DBKIND;
    uName: DBIDNAME;
  end;
  PDBIDArray = ^TDBIDArray;
  TDBIDArray = array[0..MAXBOUND] of DBID;
  PDBColumnInfo = ^TDBColumnInfo;
  TDBColumnInfo = record
    pwszName: PWideChar;
    pTypeInfo: ITypeInfo;
    iOrdinal: DBORDINAL;
    dwFlags: DBCOLUMNFLAGS;
    ulColumnSize: PtrUInt;
    wType: DBTYPE;
    bPrecision: Byte;
    bScale: Byte;
    columnid: DBID;
  end;
  DBSOURCETYPE = DWORD;
  PDBSOURCETYPE = ^DBSOURCETYPE;
  TDBProp = record
    dwPropertyID: DBPROPID;
    dwOptions: DBPROPOPTIONS;
    dwStatus: DBPROPSTATUS;
    colid: DBID;
    vValue: OleVariant;
  end;
  PDBPropArray = ^TDBPropArray;
  TDBPropArray = array[0..MAXBOUND] of TDBProp;
  TDBPropSet = record
    rgProperties: PDBPropArray;
    cProperties: UINT;
    guidPropertySet: TGUID;
  end;
  PDBPropSet = ^TDBPropSet;
  PDBPropSetArray = ^TDBPropSetArray;
  TDBPropSetArray = array[0..MAXBOUND] of TDBPropSet;
  TDBSchemaRec = record
    SchemaGuid: TGuid;
    SupportedRestrictions: Integer;
  end;
  TSSPARAMPROPS = record
    iOrdinal: DBORDINAL;
    cPropertySets: ULONG;
    rgPropertySets: PDBPropSet;
  end;
  PSSPARAMPROPS = ^TSSPARAMPROPS;
  PSSPARAMPROPSArray = ^TSSPARAMPROPSArray;
  TSSPARAMPROPSArray = array[0..MAXBOUND] of TSSPARAMPROPS;
  TSSPARAMPROPSDynArray = array of TSSPARAMPROPS;

  PDBParamInfo = ^TDBParamInfo;
  DBPARAMINFO = record
    dwFlags: UINT;
    iOrdinal: DBORDINAL;
    pwszName: PWideChar;
    pTypeInfo: ITypeInfo;
    ulParamSize: DBLENGTH;
    wType: DBTYPE;
    bPrecision: Byte;
    bScale: Byte;
  end;
  TDBParamInfo = DBPARAMINFO;

  PUintArray = ^TUintArray;
  TUintArray = array[0..MAXBOUND] of UINT;
  TUintDynArray = array of UINT;

  PDBParamBindInfo = ^TDBParamBindInfo;
  DBPARAMBINDINFO = record
    pwszDataSourceType: PWideChar;
    pwszName: PWideChar;
    ulParamSize: DBLENGTH;
    dwFlags: DBPARAMFLAGS;
    bPrecision: Byte;
    bScale: Byte;
  end;
  TDBParamBindInfo = DBPARAMBINDINFO;

  PDBParamBindInfoArray = ^TDBParamBindInfoArray;
  TDBParamBindInfoArray = array[0..MAXBOUND] of TDBParamBindInfo;
  TDBParamBindInfoDynArray = array of TDBParamBindInfo;

{$ifndef CPU64}
  {$A-} // packed records
{$endif}

  /// initialize and uninitialize OleDB data source objects and enumerators
  IDBInitialize = interface(IUnknown)
    ['{0C733A8B-2A1C-11CE-ADE5-00AA0044773D}']
    function Initialize: HResult; stdcall;
    function Uninitialize: HResult; stdcall;
  end;
  /// create an OleDB data source object using a connection string
  IDataInitialize = interface(IUnknown)
    ['{2206CCB1-19C1-11D1-89E0-00C04FD7A829}']
    function GetDataSource(const pUnkOuter: IUnknown; dwClsCtx: DWORD;
      pwszInitializationString: POleStr; const riid: TIID;
      var DataSource: IUnknown): HResult; stdcall;
    function GetInitializationString(const DataSource: IUnknown;
      fIncludePassword: Boolean; out pwszInitString: POleStr): HResult; stdcall;
    function CreateDBInstance(const clsidProvider: TGUID;
      const pUnkOuter: IUnknown; dwClsCtx: DWORD; pwszReserved: POleStr;
      riid: TIID; var DataSource: IUnknown): HResult; stdcall;
    function CreateDBInstanceEx(const clsidProvider: TGUID;
      const pUnkOuter: IUnknown; dwClsCtx: DWORD; pwszReserved: POleStr;
      pServerInfo: PCoServerInfo; cmq: ULONG; rgmqResults: PMultiQI): HResult; stdcall;
    function LoadStringFromStorage(pwszFileName: POleStr;
      out pwszInitializationString: POleStr): HResult; stdcall;
    function WriteStringToStorage(pwszFileName, pwszInitializationString: POleStr;
      dwCreationDisposition: DWORD): HResult; stdcall;
  end;
  /// obtain a new session to a given OleDB data source
  IDBCreateSession = interface(IUnknown)
    ['{0C733A5D-2A1C-11CE-ADE5-00AA0044773D}']
    function CreateSession(const punkOuter: IUnknown; const riid: TGUID;
      out ppDBSession: IUnknown): HResult; stdcall;
  end;
  /// commit, abort, and obtain status information about OleDB transactions
  ITransaction = interface(IUnknown)
    ['{0FB15084-AF41-11CE-BD2B-204C4F4F5020}']
    function Commit(fRetaining: BOOL; grfTC: UINT; grfRM: UINT): HResult; stdcall;
    function Abort(pboidReason: PBOID; fRetaining: BOOL; fAsync: BOOL): HResult; stdcall;
    function GetTransactionInfo(out pinfo: TXactTransInfo): HResult; stdcall;
  end;
  /// gets and sets a suite of options associated with an OleDB transaction
  ITransactionOptions = interface(IUnknown)
    ['{3A6AD9E0-23B9-11CF-AD60-00AA00A74CCD}']
    function SetOptions(var pOptions: TXactOpt): HResult; stdcall;
    function GetOptions(var pOptions: TXactOpt): HResult; stdcall;
  end;
  /// optional interface on OleDB sessions, used to start, commit, and abort
  // transactions on the session
  ITransactionLocal = interface(ITransaction)
    ['{0C733A5F-2A1C-11CE-ADE5-00AA0044773D}']
    function GetOptionsObject(out ppOptions: ITransactionOptions): HResult; stdcall;
    function StartTransaction(isoLevel: Integer; isoFlags: UINT;
      const pOtherOptions: ITransactionOptions; pulTransactionLevel: PUINT): HResult; stdcall;
  end;
  /// provide methods to execute commands
  ICommand = interface(IUnknown)
    ['{0C733A63-2A1C-11CE-ADE5-00AA0044773D}']
    function Cancel: HResult; stdcall;
    function Execute(const punkOuter: IUnknown; const riid: TGUID; var pParams: TDBParams;
      pcRowsAffected: PInteger; ppRowset: PIUnknown): HResult; stdcall;
    function GetDBSession(const riid: TGUID; out ppSession: IUnknown): HResult; stdcall;
  end;
  /// methods to access the ICommand text to be executed
  ICommandText = interface(ICommand)
    ['{0C733A27-2A1C-11CE-ADE5-00AA0044773D}']
    function GetCommandText(var pguidDialect: TGUID;
      out ppwszCommand: PWideChar): HResult; stdcall;
    function SetCommandText(const guidDialect: TGUID;
      pwszCommand: PWideChar): HResult; stdcall;
  end;

  ICommandWithParameters = interface(IUnknown)
    ['{0C733A64-2A1C-11CE-ADE5-00AA0044773D}']
    function GetParameterInfo(var pcParams: UINT; out prgParamInfo: PDBPARAMINFO;
      ppNamesBuffer: PPOleStr): HResult; stdcall;
    function MapParameterNames(cParamNames: DB_UPARAMS; rgParamNames: POleStrList;
      rgParamOrdinals: PPtrUIntArray): HResult; stdcall;
    function SetParameterInfo(cParams: DB_UPARAMS; rgParamOrdinals: PPtrUIntArray;
      rgParamBindInfo: PDBParamBindInfoArray): HResult; stdcall;
  end;

  ISSCommandWithParameters = interface(ICommandWithParameters)
    ['{EEC30162-6087-467C-B995-7C523CE96561}']
    function GetParameterProperties(var pcParams: PtrUInt; var prgParamProperties: PSSPARAMPROPS): HResult; stdcall;
    function SetParameterProperties (cParams: PtrUInt; prgParamProperties: PSSPARAMPROPS): HResult; stdcall;
  end;

  /// provides methods for fetching rows sequentially, getting the data from
  // those rows, and managing rows
  IRowset = interface(IUnknown)
    ['{0C733A7C-2A1C-11CE-ADE5-00AA0044773D}']
    /// Adds a reference count to an existing row handle
    function AddRefRows(cRows: PtrUInt; rghRows: PPtrUIntArray;
      rgRefCounts, rgRowStatus: PCardinalArray): HResult; stdcall;
    /// Retrieves data from the rowset's copy of the row
    function GetData(HROW: HROW; HACCESSOR: HACCESSOR; pData: Pointer): HResult; stdcall;
    /// Fetches rows sequentially, remembering the previous position
    // - this method has been modified from original OleDB.pas to allow direct
    // typecast of prghRows parameter to pointer(fRowStepHandles)
    function GetNextRows(hReserved: HCHAPTER; lRowsOffset: PtrInt; cRows: PtrInt;
      out pcRowsObtained: PtrUInt; var prghRows: pointer): HResult; stdcall;
    /// Releases rows
    function ReleaseRows(cRows: UINT; rghRows: PPtrUIntArray; rgRowOptions,
      rgRefCounts, rgRowStatus: PCardinalArray): HResult; stdcall;
    /// Repositions the next fetch position to its initial position
    // - that is, its position when the rowset was first created
    function RestartPosition(hReserved: HCHAPTER): HResult; stdcall;
  end;

  IOpenRowset = interface(IUnknown)
    ['{0C733A69-2A1C-11CE-ADE5-00AA0044773D}']
    function OpenRowset(const punkOuter: IUnknown; pTableID: PDBID; pIndexID: PDBID;
      const riid: TGUID; cPropertySets: UINT; rgPropertySets: PDBPropSetArray;
      ppRowset: PIUnknown): HResult; stdcall;
  end;

  IMultipleResults = interface(IUnknown)
  ['{0c733a8c-2a1c-11ce-ade5-00aa0044773d}']
    function GetResult(const pUnkOuter: IUnknown; lResultFlag: DBRESULTFLAG;
    const riid: TIID; pcRowsAffected: PInteger;ppRowset: PIUnknown): HResult; stdcall;
  end;

  /// interface used to retrieve enhanced custom error information
  IErrorRecords = interface(IUnknown)
    ['{0c733a67-2a1c-11ce-ade5-00aa0044773d}']
    function AddErrorRecord(pErrorInfo: PErrorInfo; dwLookupID: UINT;
      pDispParams: pointer; const punkCustomError: IUnknown;
      dwDynamicErrorID: UINT): HResult; stdcall;
    function GetBasicErrorInfo(ulRecordNum: UINT;
      pErrorInfo: PErrorInfo): HResult; stdcall;
    function GetCustomErrorObject(ulRecordNum: UINT;
      const riid: TGUID; var ppObject: IUnknown): HResult; stdcall;
    function GetErrorInfo(ulRecordNum: UINT; lcid: LCID;
      var ppErrorInfo: IErrorInfo): HResult; stdcall;
    function GetErrorParameters(ulRecordNum: UINT;
      pDispParams: pointer): HResult; stdcall;
    function GetRecordCount(var pcRecords: UINT): HResult; stdcall;
  end;
  /// used on an OleDB session to obtain a new command
  IDBCreateCommand = interface(IUnknown)
    ['{0C733A1D-2A1C-11CE-ADE5-00AA0044773D}']
    function CreateCommand(const punkOuter: IUnknown; const riid: TGUID;
      out ppCommand: ICommand): HResult; stdcall;
  end;
  /// provides methods for accessor management, to access OleDB data
  // - An accessor is a data structure created by the consumer that describes
  // how row or parameter data from the data store is to be laid out in the
  // consumer's data buffer.
  // - For each column in a row (or parameter in a set of parameters), the
  // accessor contains a binding. A binding is a DBBinding data structure that
  // holds information about a column or parameter value, such as its ordinal
  // value, data type, and destination in the consumer's buffer.
  IAccessor = interface(IUnknown)
    ['{0C733A8C-2A1C-11CE-ADE5-00AA0044773D}']
    function AddRefAccessor(HACCESSOR: HACCESSOR; pcRefCount: PUINT): HResult; stdcall;
    function CreateAccessor(dwAccessorFlags: UINT; cBindings: PtrUInt; rgBindings: PDBBindingArray;
      cbRowSize: PtrUInt; var phAccessor: HACCESSOR; rgStatus: PCardinalArray): HResult; stdcall;
    function GetBindings(HACCESSOR: HACCESSOR; pdwAccessorFlags: PUINT; var pcBindings: PtrUInt;
      out prgBindings: PDBBinding): HResult; stdcall;
    function ReleaseAccessor(HACCESSOR: HACCESSOR; pcRefCount: PUINT): HResult; stdcall;
  end;
  /// expose information about columns of an OleDB rowset or prepared command
  IColumnsInfo = interface(IUnknown)
    ['{0C733A11-2A1C-11CE-ADE5-00AA0044773D}']
    function GetColumnInfo(var pcColumns: PtrUInt; out prgInfo: PDBColumnInfo;
      out ppStringsBuffer: PWideChar): HResult; stdcall;
    function MapColumnIDs(cColumnIDs: PtrUInt; rgColumnIDs: PDBIDArray;
      rgColumns: PPtrUIntArray): HResult; stdcall;
  end;
  /// allows the display of the data link dialog boxes programmatically
  IDBPromptInitialize = interface(IUnknown)
    ['{2206CCB0-19C1-11D1-89E0-00C04FD7A829}']
    function PromptDataSource(const pUnkOuter: IUnknown; hWndParent: HWND;
      dwPromptOptions: UINT; cSourceTypeFilter: ULONG;
      rgSourceTypeFilter: PDBSOURCETYPE; pszProviderFilter: POleStr;
      const riid: TIID; var DataSource: IUnknown): HResult; stdcall;
    function PromptFileName(hWndParent: HWND; dwPromptOptions: UINT;
      pwszInitialDirectory, pwszInitialFile: POleStr;
      var ppwszSelectedFile: POleStr): HResult; stdcall;
  end;
  /// used to retrieve the database metadata (e.g. tables and fields layout)
  IDBSchemaRowset = interface(IUnknown)
    ['{0c733a7b-2a1c-11ce-ade5-00aa0044773d}']
    function GetRowset(pUnkOuter: IUnknown; const rguidSchema: TGUID;
      cRestrictions: Integer; rgRestrictions: pointer;
      const riid: TIID; cPropertySets: Integer; rgPropertySets: PDBPROPSET;
      var ppRowset: IRowset): HResult; stdcall;
    function GetSchemas(var pcSchemas: Integer; var prgSchemas: PGUID;
      var prgRestrictionSupport: PInteger): HResult; stdcall;
  end;


{ -------------- TOleDB* OleDB classes and types }

type
  /// generic Exception type, generated for OleDB connection
  EOleDBException = class(ESQLDBException);

  TOleDBConnection = class;

  TOleDBOnCustomError = function(Connection: TOleDBConnection;
    ErrorRecords: IErrorRecords; RecordNum: UINT): boolean of object;

  /// will implement properties shared by OleDB connections
  TOleDBConnectionProperties = class(TSQLDBConnectionPropertiesThreadSafe)
  protected
    fProviderName: RawUTF8;
    fConnectionString: SynUnicode;
    fOnCustomError: TOleDBOnCustomError;
    fSchemaRec: array of TDBSchemaRec;
    fSupportsOnlyIRowset: boolean;
    function GetSchema(const aUID: TGUID; const Fields: array of RawUTF8;
      var aResult: IRowSet): boolean;
    /// will create the generic fConnectionString from supplied parameters
    procedure SetInternalProperties; override;
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - used by GetForeignKey method
    procedure GetForeignKeys; override;
    /// create the database
    // - shall be called only if necessary (e.g. for file-based database, if
    // the file does not exist yet)
    function CreateDatabase: boolean; virtual;
  public
    /// create a new connection
    // - call this method if the shared MainConnection is not enough (e.g. for
    // multi-thread access)
    // - the caller is responsible of freeing this instance
    // - this overridden method will create an TOleDBConnection instance
    function NewConnection: TSQLDBConnection; override;
    /// display the OleDB/ADO Connection Settings dialog to customize the
    // OleDB connection string
    // - returns TRUE if the connection string has been modified
    // - Parent is an optional GDI Window Handle for modal display
    function ConnectionStringDialogExecute(Parent: HWND=0): boolean;
    /// get all table names
    // - will retrieve the corresponding metadata from OleDB interfaces if SQL
    // direct access was not defined
    procedure GetTableNames(out Tables: TRawUTF8DynArray); override;
    /// retrieve the column/field layout of a specified table
    // - will retrieve the corresponding metadata from OleDB interfaces if SQL
    // direct access was not defined
    procedure GetFields(const aTableName: RawUTF8; out Fields: TSQLDBColumnDefineDynArray); override;
    /// convert a textual column data type, as retrieved e.g. from SQLGetField,
    // into our internal primitive types
    function ColumnTypeNativeToDB(const aNativeType: RawUTF8; aScale: integer): TSQLDBFieldType; override;
    /// the associated OleDB connection string
    // - is set by the Create() constructor most of the time from the supplied
    // server name, user id and password, according to the database provider
    // corresponding to the class
    // - you may want to customize it via the ConnectionStringDialogExecute
    // method, or to provide some additional parameters
    property ConnectionString: SynUnicode read fConnectionString write fConnectionString;
    /// custom Error handler for OleDB COM objects
    // - returns TRUE if specific error was retrieved and has updated
    // ErrorMessage and InfoMessage
    // - default implementation just returns false
    property OnCustomError: TOleDBOnCustomError read fOnCustomError write fOnCustomError;
  published { to be loggged as JSON }
    /// the associated OleDB provider name, as set for each class
    property ProviderName: RawUTF8 read fProviderName;
  end;

  /// OleDB connection properties to an Oracle database using Oracle's Provider
  // - this will use the native OleDB provider supplied by Oracle
  // see @http://download.oracle.com/docs/cd/E11882_01/win.112/e17726/toc.htm
  TOleDBOracleConnectionProperties = class(TOleDBConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'OraOLEDB.Oracle.1'
    procedure SetInternalProperties; override;
  end;

  /// OleDB connection properties to an Oracle database using Microsoft's Provider
  // - this will use the generic (older) OleDB provider supplied by Microsoft
  // which would not be used any more:
  // "This feature will be removed in a future version of Windows. Avoid
  // using this feature in new development work, and plan to modify applications
  // that currently use this feature. Instead, use Oracle's OLE DB provider."
  // see http://msdn.microsoft.com/en-us/library/ms675851
  TOleDBMSOracleConnectionProperties = class(TOleDBOracleConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'MSDAORA'
    procedure SetInternalProperties; override;
  end;

  /// OleDB connection properties to Microsoft SQL Server 2008-2012, via
  // SQL Server Native Client 10.0 (SQL Server 2008)
  // - this will use the native OleDB provider supplied by Microsoft
  // see http://msdn.microsoft.com/en-us/library/ms677227
  // - is aUserID='' at Create, it will use Windows Integrated Security
  // for the connection
  // - will use the SQLNCLI10 provider, which will work on Windows XP;
  // if you want all features, especially under MS SQL 2012, use the
  // inherited class TOleDBMSSQL2012ConnectionProperties; if, on the other
  // hand, you need to connect to a old MS SQL Server 2005, use
  // TOleDBMSSQL2005ConnectionProperties, or set your own provider string
  TOleDBMSSQLConnectionProperties = class(TOleDBConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'SQLNCLI10'
    procedure SetInternalProperties; override;
    /// custom Error handler for OleDB COM objects
    // - will handle Microsoft SQL Server error messages (if any)
    function MSOnCustomError(Connection: TOleDBConnection;
      ErrorRecords: IErrorRecords; RecordNum: UINT): boolean;
  public
  end;

  /// OleDB connection properties to Microsoft SQL Server 2005, via
  // SQL Server Native Client (SQL Server 2005)
  // - this overridden version will use the SQLNCLI provider, which is
  // deprecated but may be an alternative with MS SQL Server 2005
  // - is aUserID='' at Create, it will use Windows Integrated Security
  // for the connection
  TOleDBMSSQL2005ConnectionProperties = class(TOleDBMSSQLConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'SQLNCLI'
    procedure SetInternalProperties; override;
  public
    /// initialize the connection properties
    // - this overridden version will disable the MultipleValuesInsert()
    // optimization as defined in TSQLDBConnectionProperties.Create(),
    // since INSERT with multiple VALUES (..),(..),(..) is available only
    // since SQL Server 2008
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8); override;
  end;

  /// OleDB connection properties to Microsoft SQL Server 2008, via
  // SQL Server Native Client 10.0 (SQL Server 2008)
  // - just maps default TOleDBMSSQLConnectionProperties type
  TOleDBMSSQL2008ConnectionProperties = TOleDBMSSQLConnectionProperties;

  /// OleDB connection properties to Microsoft SQL Server 2008/2012, via
  // SQL Server Native Client 11.0 (Microsoft SQL Server 2012 Native Client)
  // - from http://www.microsoft.com/en-us/download/details.aspx?id=29065 get
  // the sqlncli.msi package corresponding to your Operating System: note that
  // the "X64 Package" will also install the 32-bit version of the client
  // - this overridden version will use newer SQLNCLI11 provider, but won't work
  // under Windows XP - in this case, it will fall back to SQLNCLI10 - see
  // http://msdn.microsoft.com/en-us/library/ms131291
  // - if aUserID='' at Create, it will use Windows Integrated Security
  // for the connection
  // - for SQL Express LocalDB edition, just use aServerName='(localdb)\v11.0'
  TOleDBMSSQL2012ConnectionProperties = class(TOleDBMSSQLConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'SQLNCLI11'
    // - will leave older 'SQLNCLI10' on Windows XP
    procedure SetInternalProperties; override;
  end;

  /// OleDB connection properties to MySQL Server
  TOleDBMySQLConnectionProperties = class(TOleDBConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'MySQLProv'
    procedure SetInternalProperties; override;
  end;

{$ifndef CPU64} // Jet is not available on Win64

  /// OleDB connection properties to Jet/MSAccess .mdb files
  // - the server name should be the .mdb file name
  // - note that the Jet OleDB driver is not available under Win64 platform
  TOleDBJetConnectionProperties = class(TOleDBConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'Microsoft.Jet.OLEDB.4.0'
    procedure SetInternalProperties; override;
  end;

{$endif}

  /// OleDB connection properties to Microsoft Access Database
  TOleDBACEConnectionProperties = class(TOleDBConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'Microsoft.ACE.OLEDB.12.0'
    procedure SetInternalProperties; override;
  end;

  /// OleDB connection properties to IBM AS/400
  TOleDBAS400ConnectionProperties = class(TOleDBConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'IBMDA400.DataSource.1'
    procedure SetInternalProperties; override;
  end;

  /// OleDB connection properties to Informix Server
  TOleDBInformixConnectionProperties = class(TOleDBConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'Ifxoledbc'
    procedure SetInternalProperties; override;
  end;

  /// OleDB connection properties via Microsoft Provider for ODBC
  // - this will use the ODBC provider supplied by Microsoft
  // see http://msdn.microsoft.com/en-us/library/ms675326(v=VS.85).aspx
  // - an ODBC Driver should be specified at creation
  // - you should better use direct connection classes, like
  // TOleDBMSSQLConnectionProperties or TOleDBOracleConnectionProperties
  // as defined in SynDBODBC.pas
  TOleDBODBCSQLConnectionProperties = class(TOleDBConnectionProperties)
  protected
    fDriver: RawUTF8;
    /// will set the appropriate provider name, i.e. 'MSDASQL'
    procedure SetInternalProperties; override;
  public
    /// initialize the properties
    // - an additional parameter is available to set the ODBC driver to use
    // - you may also set aDriver='' and modify the connection string directly,
    // e.g. adding '{ DSN=name | FileDSN=filename };'
    constructor Create(const aDriver, aServerName, aDatabaseName,
      aUserID, aPassWord: RawUTF8); reintroduce;
  published { to be logged as JSON }
    /// the associated ODBC Driver name, as specified at creation
    property Driver: RawUTF8 read fDriver;
  end;

  /// implements an OleDB connection
  // - will retrieve the remote DataBase behavior from a supplied
  // TSQLDBConnectionProperties class, shared among connections
  TOleDBConnection = class(TSQLDBConnectionThreadSafe)
  protected
    fMalloc: IMalloc;
    fDBInitialize: IDBInitialize;
    fTransaction: ITransactionLocal;
    fSession: IUnknown;
    fOleDBProperties: TOleDBConnectionProperties;
    fOleDBErrorMessage, fOleDBInfoMessage: string;
    /// Error handler for OleDB COM objects
    // - will update ErrorMessage and InfoMessage
    procedure OleDBCheck(aStmt: TSQLDBStatement; aResult: HRESULT;
      const aStatus: TCardinalDynArray=nil); virtual;
    /// called just after fDBInitialize.Initialized: could add parameters
    procedure OnDBInitialized; virtual;
  public
    /// connect to a specified OleDB database
    constructor Create(aProperties: TSQLDBConnectionProperties); override;
    /// release all associated memory and OleDB COM objects
    destructor Destroy; override;
    /// initialize a new SQL query statement for the given connection
    // - the caller should free the instance after use
    function NewStatement: TSQLDBStatement; override;
    /// connect to the specified database
    // - should raise an EOleDBException on error
    procedure Connect; override;
    /// stop connection to the specified database
    // - should raise an EOleDBException on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;
    /// begin a Transaction for this connection
    // - be aware that not all OleDB provider support nested transactions
    // see http://msdn.microsoft.com/en-us/library/ms716985(v=vs.85).aspx
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;
    /// the associated OleDB database properties
    property OleDBProperties: TOleDBConnectionProperties read fOleDBProperties;
    /// internal error message, as retrieved from the OleDB provider
    property OleDBErrorMessage: string read fOleDBErrorMessage;
    /// internal information message, as retrieved from the OleDB provider
    property OleDBInfoMessage: string read fOleDBInfoMessage;
  end;

  /// used to store properties and value about one TOleDBStatement Param
  // - we don't use a Variant, not the standard TSQLDBParam record type,
  // but manual storage for better performance
  // - whole memory block of a TOleDBStatementParamDynArray will be used as the
  // source Data for the OleDB parameters - so we should align data carefully
  {$ifdef CPU64}
    {$A8} // un-packed records
  {$else}
    {$A-} // packed records
  {$endif}
  TOleDBStatementParam = record
    /// storage used for BLOB (ftBlob) values
    // - will be refered as DBTYPE_BYREF when sent as OleDB parameters, to
    // avoid unnecessary memory copy
    VBlob: RawByteString;
    /// storage used for TEXT (ftUTF8) values
    // - we store TEXT here as WideString, and not RawUTF8, since OleDB
    // expects the text to be provided with Unicode encoding
    // - for some providers (like Microsoft SQL Server 2008 R2, AFAIK), using
    // DBTYPE_WSTR value (i.e. what the doc. says) will raise an OLEDB Error
    // 80040E1D (DB_E_UNSUPPORTEDCONVERSION, i.e. 'Requested conversion is not
    // supported'): we found out that only DBTYPE_BSTR type (i.e. OLE WideString)
    // does work... so we'll use it here! Shame on Microsoft!
    // - what's fine with DBTYPE_BSTR is that it can be resized by the provider
    // in case of VInOut in [paramOut, paramInOut] - so let it be
    VText: WideString;
    /// storage used for ftInt64, ftDouble, ftDate and ftCurrency value
    VInt64: Int64;
    /// storage used for table variables
    VIUnknown: IUnknown;
    /// storage used for table variables
    VArray: TRawUTF8DynArray;
    /// storage used for the OleDB status field
    // - if VStatus=ord(stIsNull), then it will bind a NULL with the type
    // as set by VType (to avoid conversion error like in [e8c211062e])
    VStatus: integer;
    /// the column/parameter Value type
    VType: TSQLDBFieldType;
    /// define if parameter can be retrieved after a stored procedure execution
    VInOut: TSQLDBParamInOutType;
    // so that VInt64 will be 8 bytes aligned
    VFill: array[sizeof(TSQLDBFieldType)+sizeof(TSQLDBParamInOutType)+sizeof(integer)..
      SizeOf(Int64)-1] of byte;
  end;
  {$ifdef CPU64}
    {$A-} // packed records
  {$endif}
  POleDBStatementParam = ^TOleDBStatementParam;

  /// used to store properties about TOleDBStatement Parameters
  // - whole memory block of a TOleDBStatementParamDynArray will be used as the
  // source Data for the OleDB parameters
  TOleDBStatementParamDynArray = array of TOleDBStatementParam;

  /// implements an OleDB SQL query statement
  // - this statement won't retrieve all rows of data, but will allow direct
  // per-row access using the Step() and Column*() methods
  TOleDBStatement = class(TSQLDBStatement)
  protected
    fParams: TOleDBStatementParamDynArray;
    fColumns: TSQLDBColumnPropertyDynArray;
    fParam: TDynArray;
    fColumn: TDynArrayHashed;
    fCommand: ICommandText;
    fRowSet: IRowSet;
    fRowSetAccessor: HACCESSOR;
    fRowSize: integer;
    fRowStepResult: HRESULT;
    fRowStepHandleRetrieved: PtrUInt;
    fRowStepHandleCurrent: PtrUInt;
    fRowStepHandles: TPtrUIntDynArray;
    fRowSetData: array of byte;
    fParamBindings: TDBBindingDynArray;
    fColumnBindings: TDBBindingDynArray;
    fHasColumnValueInlined: boolean;
    fOleDBConnection: TOleDBConnection;
    fDBParams: TDBParams;
    fRowBufferSize: integer;
    fUpdateCount: integer;
    fAlignBuffer: boolean;
    procedure SetRowBufferSize(Value: integer);
    /// resize fParams[] if necessary, set the VType and return pointer to
    // the corresponding entry in fParams[]
    // - first parameter has Param=1
    function CheckParam(Param: Integer; NewType: TSQLDBFieldType;
      IO: TSQLDBParamInOutType): POleDBStatementParam; overload;
    function CheckParam(Param: Integer; NewType: TSQLDBFieldType;
      IO: TSQLDBParamInOutType; ArrayCount: integer): POleDBStatementParam; overload;
    /// raise an exception if Col is incorrect or no IRowSet is available
    // - set Column to the corresponding fColumns[] item
    // - return a pointer to status-data[-length] in fRowSetData[], or
    // nil if status states this column is NULL
    function GetCol(Col: integer; out Column: PSQLDBColumnProperty): pointer;
    procedure GetCol64(Col: integer; DestType: TSQLDBFieldType; var Dest);
      {$ifdef HASINLINE}inline;{$endif}
    procedure FlushRowSetData;
    procedure ReleaseRowSetDataAndRows;
    procedure CloseRowSet;
    ///  retrieve column information, and initialize Bindings[]
    // - add the high-level column information in Column[], initializes
    // OleDB Bindings array and returns the row size (in bytes)
    function BindColumns(ColumnInfo: IColumnsInfo; var Column: TDynArrayHashed;
      out Bindings: TDBBindingDynArray): integer;
    procedure LogStatusError(Status: integer; Column: PSQLDBColumnProperty);
  public
    /// create an OleDB statement instance, from an OleDB connection
    // - the Execute method can be called only once per TOleDBStatement instance
    // - if the supplied connection is not of TOleDBConnection type, will raise
    // an exception
    constructor Create(aConnection: TSQLDBConnection); override;
    /// release all associated memory and COM objects
    destructor Destroy; override;
    /// retrieve column information from a supplied IRowSet
    // - is used e.g. by TOleDBStatement.Execute or to retrieve metadata columns
    // - raise an exception on error
    procedure FromRowSet(RowSet: IRowSet);

    /// bind a NULL value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - OleDB during MULTI INSERT statements expect BoundType to be set in
    // TOleDBStatementParam, and its VStatus set to ord(stIsNull)
    // - raise an EOleDBException on any error
    procedure BindNull(Param: Integer; IO: TSQLDBParamInOutType=paramIn;
      BoundType: TSQLDBFieldType=ftNull); override;
    /// bind an array of Int64 values to a parameter
    // - using TABLE variable (MSSQl 2008 & UP). Must be created in the database as:
    // $ CREATE TYPE dbo.IDList AS TABLE(id bigint NULL)
    // - Internally BindArray(0, [1, 2,3]) is the same as:
    // $ declare @a dbo.IDList;
    // $ insert into @a (id) values (1), (2), (3);
    // $ SELECT usr.ID   FROM user usr WHERE usr.ID IN  (select id from @a)
    procedure BindArray(Param: Integer;
      const Values: array of Int64); overload; override;
    /// bind a array of RawUTF8 (255 length max) values to a parameter
    // - using TABLE variable (MSSQl 2008 & UP). Must be created in the database as:
    // $ CREATE TYPE dbo.StrList AS TABLE(id nvarchar(255) NULL)
    // - must be declareded in the database
    procedure BindArray(Param: Integer;
      const Values: array of RawUTF8); overload; override;
    /// bind an integer value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure Bind(Param: Integer; Value: Int64;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    /// bind a double value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure Bind(Param: Integer; Value: double;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    /// bind a TDateTime value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure BindDateTime(Param: Integer; Value: TDateTime;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    /// bind a currency value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure BindCurrency(Param: Integer; Value: currency;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure BindTextU(Param: Integer; const Value: RawUTF8;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    /// bind a UTF-8 encoded buffer text (#0 ended) to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure BindTextP(Param: Integer; Value: PUTF8Char;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    /// bind a VCL string to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure BindTextS(Param: Integer; const Value: string;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    /// bind an OLE WideString to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure BindTextW(Param: Integer; const Value: WideString;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure BindBlob(Param: Integer; Data: pointer; Size: integer;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure BindBlob(Param: Integer; const Data: RawByteString;
      IO: TSQLDBParamInOutType=paramIn); overload; override;

    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - raise an EOleDBException on any error
    procedure Prepare(const aSQL: RawUTF8; ExpectResults: Boolean=false); overload; override;
    /// Execute an UTF-8 encoded SQL statement
    // - parameters marked as ? should have been already bound with Bind*()
    // functions above
    // - raise an EOleDBException on any error
    procedure ExecutePrepared; override;
    /// Reset the previous prepared statement
    // - this overridden implementation will reset all bindings and the cursor state
    // - raise an EOleDBException on any error
    procedure Reset; override;
    /// gets a number of updates made by latest executed statement
    function UpdateCount: integer; override;

    /// retrieve the parameter content, after SQL execution
    // - the leftmost SQL parameter has an index of 1
    // - to be used e.g. with stored procedures
    // - any TEXT parameter will be retrieved as WideString Variant (i.e. as
    // stored in TOleDBStatementParam)
    function ParamToVariant(Param: Integer; var Value: Variant;
      CheckIsOutParameter: boolean=true): TSQLDBFieldType; override;

    /// after a statement has been prepared via Prepare() + ExecutePrepared() or
    // Execute(), this method must be called one or more times to evaluate it
    // - you shall call this method before calling any Column*() methods
    // - return TRUE on success, with data ready to be retrieved by Column*()
    // - return FALSE if no more row is available (e.g. if the SQL statement
    // is not a SELECT but an UPDATE or INSERT command)
    // - access the first or next row of data from the SQL Statement result:
    // if SeekFirst is TRUE, will put the cursor on the first row of results,
    // otherwise, it will fetch one row of data, to be called within a loop
    // - raise an ESQLEOleDBException on any error
    function Step(SeekFirst: boolean=false): boolean; override;
    /// clear result rowset when ISQLDBStatement is back in cache
    procedure ReleaseRows; override;
    /// retrieve a column name of the current Row
    // - Columns numeration (i.e. Col value) starts with 0
    // - it's up to the implementation to ensure than all column names are unique
    function ColumnName(Col: integer): RawUTF8; override;
    /// returns the Column index of a given Column name
    // - Columns numeration (i.e. Col value) starts with 0
    // - returns -1 if the Column name is not found (via case insensitive search)
    function ColumnIndex(const aColumnName: RawUTF8): integer; override;
    /// the Column type of the current Row
    // - ftCurrency type should be handled specificaly, for faster process and
    // avoid any rounding issue, since currency is a standard OleDB type
    // - FieldSize can be set to store the size in chars of a ftUTF8 column
    // (0 means BLOB kind of TEXT column)
    function ColumnType(Col: integer; FieldSize: PInteger=nil): TSQLDBFieldType; override;
    /// returns TRUE if the column contains NULL
    function ColumnNull(Col: integer): boolean; override;
    /// return a Column integer value of the current Row, first Col is 0
    function ColumnInt(Col: integer): Int64; override;
    /// return a Column floating point value of the current Row, first Col is 0
    function ColumnDouble(Col: integer): double; override;
    /// return a Column date and time value of the current Row, first Col is 0
    function ColumnDateTime(Col: integer): TDateTime; override;
    /// return a Column currency value of the current Row, first Col is 0
    // - should retrieve directly the 64 bit Currency content, to avoid
    // any rounding/conversion error from floating-point types
    function ColumnCurrency(Col: integer): currency; override;
    /// return a Column UTF-8 encoded text value of the current Row, first Col is 0
    function ColumnUTF8(Col: integer): RawUTF8; override;
    /// return a Column text generic VCL string value of the current Row, first Col is 0
    function ColumnString(Col: integer): string; override;
    /// return a Column as a blob value of the current Row, first Col is 0
    // - ColumnBlob() will return the binary content of the field is was not ftBlob,
    // e.g. a 8 bytes RawByteString for a vtInt64/vtDouble/vtDate/vtCurrency,
    // or a direct mapping of the RawUnicode
    function ColumnBlob(Col: integer): RawByteString; override;
    /// append all columns values of the current Row to a JSON stream
    // - will use WR.Expand to guess the expected output format
    // - fast overridden implementation with no temporary variable
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"
    // format and contains true BLOB data
    procedure ColumnsToJSON(WR: TJSONWriter); override;
    /// return a Column as a variant
    // - this implementation will retrieve the data with no temporary variable
    // (since TQuery calls this method a lot, we tried to optimize it)
    // - a ftUTF8 content will be mapped into a generic WideString variant
    // for pre-Unicode version of Delphi, and a generic UnicodeString (=string)
    // since Delphi 2009: you may not loose any data during charset conversion
    // - a ftBlob content will be mapped into a TBlobData AnsiString variant
    function ColumnToVariant(Col: integer; var Value: Variant): TSQLDBFieldType; override;
    /// just map the original Collection into a TOleDBConnection class
    property OleDBConnection: TOleDBConnection read fOleDBConnection;
    /// if TRUE, the data will be 8 bytes aligned in OleDB internal buffers
    // - it's recommended by official OleDB documentation for faster process
    // - is enabled by default, and should not be modified in most cases
    property AlignDataInternalBuffer: boolean read fAlignBuffer write fAlignBuffer;
    /// size in bytes of the internal OleDB buffer used to fetch rows
    // - several rows are retrieved at once into the internal buffer
    // - default value is 16384 bytes, minimal allowed size is 8192
    property RowBufferSize: integer read fRowBufferSize write SetRowBufferSize;
  end;

  TBaseAggregatingRowset = class(TObject, IUnknown, IRowset)
  private
   fcTotalRows: UINT;
   // Defining as an array because in general there can be as many accessors as necessary
   // the reading rules from the provider for such scenarios are describe in the Books online
   fhAccessor: HACCESSORDynArray;
  protected
   fidxRow: UINT;
   fUnkInnerSQLNCLIRowset: IUnknown;
   // Save the handle of the accessor that we create, the indexing is 0 based
   procedure SetAccessorHandle(idxAccessor: ULONG; hAccessor: HACCESSOR );
  public
    constructor Create(cTotalRows: UINT);
    function SetupAccessors(pIAccessorTVP: IAccessor):HRESULT; virtual; abstract;
    destructor Destroy; override;
    {$ifdef FPC}
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    {$else}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    {$endif}
    /// Adds a reference count to an existing row handle
    function AddRefRows(cRows: PtrUInt; rghRows: PPtrUIntArray;
      rgRefCounts, rgRowStatus: PCardinalArray): HResult; stdcall;
    /// Retrieves data from the rowset's copy of the row
    function GetData(HROW: HROW; HACCESSOR: HACCESSOR; pData: Pointer): HResult; virtual; stdcall;
    /// Fetches rows sequentially, remembering the previous position
    // - this method has been modified from original OleDB.pas to allow direct
    // typecast of prghRows parameter to pointer(fRowStepHandles)
    function GetNextRows(hReserved: HCHAPTER; lRowsOffset: PtrInt; cRows: PtrInt;
      out pcRowsObtained: PtrUInt; var prghRows: pointer): HResult; stdcall;
    /// Releases rows
    function ReleaseRows(cRows: UINT; rghRows: PPtrUIntArray; rgRowOptions,
      rgRefCounts, rgRowStatus: PCardinalArray): HResult; stdcall;
    /// Repositions the next fetch position to its initial position
    // - that is, its position when the rowset was first created
    function RestartPosition(hReserved: HCHAPTER): HResult; stdcall;
  end;

  TIDListRec = record
    IDLen: PtrUInt;
    IDST: DBSTATUS;
    IDVal: int64;
    StrVal: PWideChar;
  end;

  PIDListRec = ^TIDListRec;

  TIDListRowset = class(TBaseAggregatingRowset)
  private
    farr: TRawUTF8DynArray;
    fType: TSQLDBFieldType;
  public
    constructor Create(arr: TRawUTF8DynArray; aType: TSQLDBFieldType);

    function Initialize(pIOpenRowset: IOpenRowset): HRESULT;
    function GetData(HROW: HROW; HACCESSOR: HACCESSOR; pData: Pointer): HResult; override; stdcall;
    function SetupAccessors(pIAccessorIDList: IAccessor):HRESULT; override;

    procedure FillRowData(pCurrentRec:PIDListRec);
    procedure FillBindingsAndSetupRowBuffer(pBindingsList: PDBBindingArray);
  end;

/// check from the file beginning if sounds like a valid Jet / MSAccess file
function IsJetFile(const FileName: TFileName): boolean;

/// this global procedure should be called for each thread needing to use OLE
// - it is already called by TOleDBConnection.Create when an OleDb connection
// is instantiated for a new thread
// - every call of CoInit shall be followed by a call to CoUninit
// - implementation will maintain some global counting, to call the CoInitialize
// API only once per thread
// - only made public for user convenience, e.g. when using custom COM objects
procedure CoInit;

/// this global procedure should be called at thread termination
// - it is already called by TOleDBConnection.Destroy, e.g. when thread associated
// to an OleDb connection is terminated
// - every call of CoInit shall be followed by a call to CoUninit
// - only made public for user convenience, e.g. when using custom COM objects
procedure CoUninit;


implementation

function IsJetFile(const FileName: TFileName): boolean;
var F: THandle;
    Header: array[0..31] of AnsiChar;
begin
  F := FileOpen(FileName,fmOpenRead or fmShareDenyNone);
  if F=INVALID_HANDLE_VALUE then
    result := false else begin
    result := (FileRead(F,Header,sizeof(Header))=SizeOf(Header)) and
      IdemPChar(@Header[4],'STANDARD JET');
    FileClose(F);
  end;
end;


{ TOleDBStatement }

procedure TOleDBStatement.BindTextU(Param: Integer; const Value: RawUTF8;
  IO: TSQLDBParamInOutType);
begin
  if (Value='') and fConnection.Properties.StoreVoidStringAsNull then
    CheckParam(Param,ftNull,IO) else
    UTF8ToWideString(Value,CheckParam(Param,ftUTF8,IO)^.VText);
end;

procedure TOleDBStatement.BindTextP(Param: Integer; Value: PUTF8Char;
  IO: TSQLDBParamInOutType);
begin
  if (Value='') and fConnection.Properties.StoreVoidStringAsNull then
    CheckParam(Param,ftNull,IO) else
    UTF8ToWideString(Value,StrLen(Value),CheckParam(Param,ftUTF8,IO)^.VText);
end;

procedure TOleDBStatement.BindTextS(Param: Integer; const Value: string;
  IO: TSQLDBParamInOutType);
begin
  if (Value='') and fConnection.Properties.StoreVoidStringAsNull then
    CheckParam(Param,ftNull,IO) else
    CheckParam(Param,ftUTF8,IO)^.VText := StringToSynUnicode(Value);
end;

procedure TOleDBStatement.BindTextW(Param: Integer;
  const Value: WideString; IO: TSQLDBParamInOutType);
begin
  if (Value='') and fConnection.Properties.StoreVoidStringAsNull then
    CheckParam(Param,ftNull,IO) else
    CheckParam(Param,ftUTF8,IO)^.VText := Value;
end;

procedure TOleDBStatement.BindBlob(Param: Integer;
  const Data: RawByteString; IO: TSQLDBParamInOutType);
begin
  CheckParam(Param,ftBlob,IO)^.VBlob := Data;
end;

procedure TOleDBStatement.BindBlob(Param: Integer; Data: pointer; Size: integer;
  IO: TSQLDBParamInOutType);
begin
  SetString(CheckParam(Param,ftBlob,IO)^.VBlob,PAnsiChar(Data),Size);
end;

procedure TOleDBStatement.Bind(Param: Integer; Value: double;
  IO: TSQLDBParamInOutType);
begin
  CheckParam(Param,ftDouble,IO)^.VInt64 := PInt64(@Value)^;
end;

procedure TOleDBStatement.BindArray(Param: Integer;
  const Values: array of Int64);
var i: integer;
begin
  with CheckParam(Param,ftInt64,paramIn,length(Values))^ do
    for i := 0 to high(Values) do
      VArray[i] := Int64ToUtf8(Values[i]);
end;

procedure TOleDBStatement.BindArray(Param: Integer;
  const Values: array of RawUTF8);
var i: integer;
    StoreVoidStringAsNull: boolean;
begin
  StoreVoidStringAsNull := fConnection.Properties.StoreVoidStringAsNull;
  with CheckParam(Param,ftUTF8,paramIn,length(Values))^ do
    for i := 0 to high(Values) do
      if StoreVoidStringAsNull and (Values[i]='') then
        VArray[i] := 'null' else
        QuotedStr(Values[i],'''',VArray[i]);
end;

procedure TOleDBStatement.Bind(Param: Integer; Value: Int64;
  IO: TSQLDBParamInOutType);
begin
  CheckParam(Param,ftInt64,IO)^.VInt64 := Value;
end;

procedure TOleDBStatement.BindCurrency(Param: Integer; Value: currency;
  IO: TSQLDBParamInOutType);
begin
  CheckParam(Param,ftCurrency,IO)^.VInt64 := PInt64(@Value)^;
end;

procedure TOleDBStatement.BindDateTime(Param: Integer; Value: TDateTime;
  IO: TSQLDBParamInOutType);
begin
  CheckParam(Param,ftDate,IO)^.VInt64 := PInt64(@Value)^;
end;

procedure TOleDBStatement.BindNull(Param: Integer;
  IO: TSQLDBParamInOutType; BoundType: TSQLDBFieldType);
begin
  CheckParam(Param,BoundType,IO)^.VStatus := ord(stIsNull);
end;

function TOleDBStatement.CheckParam(Param: Integer; NewType: TSQLDBFieldType;
  IO: TSQLDBParamInOutType): POleDBStatementParam;
begin
  if Param<=0 then
    raise EOleDBException.CreateUTF8(
      '%.Bind*() called with Param=% should be >= 1',[self,Param]);
  if Param>fParamCount then
    fParam.Count := Param; // resize fParams[] dynamic array if necessary
  result := @fParams[Param-1];
  result^.VType := NewType;
  result^.VInOut := IO;
  result^.VStatus := 0;
end;

function TOleDBStatement.CheckParam(Param: Integer; NewType: TSQLDBFieldType;
  IO: TSQLDBParamInOutType; ArrayCount: integer): POleDBStatementParam;
begin
  result := CheckParam(Param,NewType,IO);
  if (NewType in [ftUnknown,ftNull]) or
     (fConnection.Properties.BatchSendingAbilities*[cCreate,cUpdate,cDelete]=[]) then
    raise ESQLDBException.CreateUTF8('Invalid call to %s.BindArray(Param=%d,Type=%s)',
      [self,Param,TSQLDBFieldTypeToString(NewType)]);
  SetLength(result^.VArray,ArrayCount);
  result^.VInt64 := ArrayCount;
end;

constructor TOleDBStatement.Create(aConnection: TSQLDBConnection);
begin
  if not aConnection.InheritsFrom(TOleDBConnection) then
    raise EOleDBException.CreateUTF8('%.Create(%) expects a TOleDBConnection',
      [self,aConnection]);
  inherited Create(aConnection);
  fOleDBConnection := TOleDBConnection(aConnection);
  fParam.Init(TypeInfo(TOleDBStatementParamDynArray),fParams,@fParamCount);
  fColumn.InitSpecific(TypeInfo(TSQLDBColumnPropertyDynArray),fColumns,djRawUTF8,@fColumnCount,True);
  fRowBufferSize := 16384;
  fAlignBuffer := true;
end;

type
  TColumnValue = packed record
    Status: PtrInt;
    Length: PtrUInt; // ignored for alignment
    case integer of
    0: (Int64: Int64);
    1: (Double: double);
    2: (case integer of
        0: (VData: array[0..0] of byte);
        1: (VWideChar: PWideChar);
        2: (VAnsiChar: PAnsiChar));
  end;
  PColumnValue = ^TColumnValue;

procedure TOleDBStatement.LogStatusError(Status: integer; Column: PSQLDBColumnProperty);
var msg: RawUTF8;
begin
{$ifndef PUREPASCAL}
  if cardinal(Status)<=cardinal(ord(high(TOleDBStatus))) then
    msg := UnCamelCase(TrimLeftLowerCaseShort(GetEnumName(TypeInfo(TOleDBStatus),Status))) else
{$else}
    Int32ToUtf8(Status,msg);
{$endif}
  SynDBLog.Add.Log(sllError,'Invalid [%] status for column [%] at row % for %',
    [msg,Column^.ColumnName,fCurrentRow,fSQL],self);
end;

function TOleDBStatement.GetCol(Col: integer; out Column: PSQLDBColumnProperty): pointer;
begin
  CheckCol(Col); // check Col value
  if not Assigned(fRowSet) or (fColumnCount=0) then
    raise EOleDBException.CreateUTF8('%.Column*() with no prior Execute',[self]);
  if CurrentRow<=0 then
    raise EOleDBException.CreateUTF8('%.Column*() with no prior Step',[self]);
  Column := @fColumns[Col];
  result := @fRowSetData[Column^.ColumnAttr];
  case TOleDBStatus(PColumnValue(result)^.Status) of
    stOk:
      exit; // valid content
    stIsNull:
      result := nil;
    stTruncated:
      LogTruncatedColumn(Column^);
    else
      LogStatusError(PColumnValue(result)^.Status,Column);
  end;
end;

procedure TOleDBStatement.GetCol64(Col: integer;
  DestType: TSQLDBFieldType; var Dest);
var C: PSQLDBColumnProperty;
    V: PColumnValue;
begin
  V := GetCol(Col,C);
  if V=nil then // column is NULL
    Int64(Dest) := 0 else
    if C^.ColumnType=DestType then
      // types match -> fast direct retrieval
      Int64(Dest) := V^.Int64 else
      // need conversion to destination type
      ColumnToTypedValue(Col,DestType,Dest);
end;

function TOleDBStatement.ColumnBlob(Col: integer): RawByteString;
// ColumnBlob will return the binary content of the field
var C: PSQLDBColumnProperty;
    V: PColumnValue;
    P: PAnsiChar;
begin
  V := GetCol(Col,C);
  if V=nil then // column is NULL
    result := '' else
    case C^.ColumnType of
    ftBlob: begin
      if C^.ColumnValueInlined then
        P := @V^.VData else
        P := V^.VAnsiChar;
      SetString(Result,P,V^.Length);
    end;
    ftUTF8:
      if V^.Length=0 then
        result := '' else begin
        if C^.ColumnValueInlined then
          P := @V^.VData else
          P := V^.VAnsiChar;
        // +1 below for trailing WideChar(#0) in the resulting RawUnicode
        SetString(Result,P,V^.Length+1);
      end;
     else SetString(result,PAnsiChar(@V^.Int64),sizeof(Int64));
    end;
end;

function TOleDBStatement.ColumnCurrency(Col: integer): currency;
begin
  GetCol64(Col,ftCurrency,Result);
end;

function TOleDBStatement.ColumnDateTime(Col: integer): TDateTime;
begin
  GetCol64(Col,ftDate,Result);
end;

function TOleDBStatement.ColumnDouble(Col: integer): double;
begin
  GetCol64(Col,ftDouble,Result);
end;

function TOleDBStatement.ColumnIndex(const aColumnName: RawUTF8): integer;
begin
  result := fColumn.FindHashed(aColumnName);
end;

function TOleDBStatement.ColumnNull(Col: integer): boolean;
var C: PSQLDBColumnProperty;
begin
  result := GetCol(Col,C)=nil;
end;

function TOleDBStatement.ColumnInt(Col: integer): Int64;
begin
  GetCol64(Col,ftInt64,Result);
end;

function TOleDBStatement.ColumnName(Col: integer): RawUTF8;
begin
  CheckCol(Col);
  result := fColumns[Col].ColumnName;
end;

function TOleDBStatement.ColumnType(Col: integer; FieldSize: PInteger=nil): TSQLDBFieldType;
begin
  CheckCol(Col);
  with fColumns[Col] do begin
    result := ColumnType;
    if FieldSize<>nil then
      if ColumnValueInlined then
        FieldSize^ := ColumnValueDBSize else
        FieldSize^ := 0;
  end;
end;

function TOleDBStatement.ColumnUTF8(Col: integer): RawUTF8;
var C: PSQLDBColumnProperty;
    V: PColumnValue;
    P: pointer;
begin
  V := GetCol(Col,C);
  if V=nil then // column is NULL
    result := '' else
    case C^.ColumnType of // fast direct conversion from OleDB buffer
    ftInt64: result := Int64ToUtf8(V^.Int64);
    ftDate:  result := DateTimeToIso8601Text(V^.Double);
    ftUTF8: begin
      if C^.ColumnValueInlined then
        P := @V^.VData else
        P := V^.VWideChar;
      result := RawUnicodeToUtf8(P,V^.Length shr 1);
    end;
    ftBlob: begin
      if C^.ColumnValueInlined then
        P := @V^.VData else
        P := V^.VAnsiChar;
      result := BinToBase64WithMagic(P,V^.Length);
    end;
    ftCurrency: result := Curr64ToStr(V^.Int64);
    ftDouble:
      if V^.Int64=0 then
        result := SmallUInt32UTF8[0] else
        result := DoubleToStr(V^.Double);
    end;
end;

function TOleDBStatement.ColumnString(Col: integer): string;
var C: PSQLDBColumnProperty;
    V: PColumnValue;
    P: pointer;
begin
  V := GetCol(Col,C);
  if V=nil then // column is NULL
    result := '' else
    case C^.ColumnType of // fast direct conversion from OleDB buffer
    ftInt64: result := IntToString(V^.Int64);
    ftDouble:
      if V^.Int64=0 then
        result := '0' else
        result := DoubleToString(V^.Double);
    ftCurrency: result := Curr64ToString(V^.Int64);
    ftDate:  result := Ansi7ToString(DateTimeToIso8601Text(V^.Double));
    ftUTF8: begin
      if C^.ColumnValueInlined then
        P := @V^.VData else
        P := V^.VWideChar;
      result := RawUnicodeToString(P,V^.Length shr 1);
    end;
    ftBlob: begin
      if C^.ColumnValueInlined then
        P := @V^.VData else
        P := V^.VAnsiChar;
      result := Ansi7ToString(BinToBase64WithMagic(P,V^.Length));
    end;
    end;
end;

function TOleDBStatement.ColumnToVariant(Col: integer;
  var Value: Variant): TSQLDBFieldType;
var C: PSQLDBColumnProperty;
    V: PColumnValue;
    P: pointer;
begin // dedicated version to avoid as much memory allocation than possible
  V := GetCol(Col,C);
  if V=nil then
    result := ftNull else
    result := C^.ColumnType;
  VarClear(Value);
  with TVarData(Value) do begin
    VType := MAP_FIELDTYPE2VARTYPE[result];
    case result of
      ftInt64, ftDouble, ftCurrency, ftDate:
        VInt64 := V^.Int64; // copy 64 bit content
      ftUTF8: begin
        VAny := nil;
        if C^.ColumnValueInlined then
          P := @V^.VData else
          P := V^.VAnsiChar;
        {$ifndef UNICODE}
        if not Connection.Properties.VariantStringAsWideString then begin
          VType := varString;
          RawUnicodeToString(P,V^.Length shr 1,AnsiString(VAny));
        end else
        {$endif}
          SetString(SynUnicode(VAny),PWideChar(P),V^.Length shr 1);
      end;
      ftBlob:
      if fForceBlobAsNull then
        VType := varNull else begin
        VAny := nil;
        if C^.ColumnValueInlined then
          P := @V^.VData else
          P := V^.VAnsiChar;
        SetString(RawByteString(VAny),PAnsiChar(P),V^.Length);
      end;
      end;
  end;
end;

procedure TOleDBStatement.ColumnsToJSON(WR: TJSONWriter);
var col: integer;
    V: PColumnValue;
    P: Pointer;
label Write;
begin // dedicated version to avoid as much memory allocation than possible
  if CurrentRow<=0 then
    raise EOleDBException.CreateUTF8('%.ColumnsToJSON() with no prior Step',[self]);
  if WR.Expand then
    WR.Add('{');
  for col := 0 to fColumnCount-1 do // fast direct conversion from OleDB buffer
  with fColumns[col] do begin
    if WR.Expand then
      WR.AddFieldName(ColumnName); // add '"ColumnName":'
    V := @fRowSetData[ColumnAttr];
    case TOleDBStatus(V^.Status) of
      stOK:
Write:case ColumnType of
        ftInt64:    WR.Add(V^.Int64);
        ftDouble:   WR.AddDouble(V^.Double);
        ftCurrency: WR.AddCurr64(V^.Int64);
        ftDate: begin
          WR.Add('"');
          WR.AddDateTime(@V^.Double,'T',#0,fForceDateWithMS);
          WR.Add('"');
        end;
        ftUTF8: begin
          WR.Add('"');
          if ColumnValueInlined then
            P := @V^.VData else
            P := V^.VWideChar;
          WR.AddJSONEscapeW(P,V^.Length shr 1);
          WR.Add('"');
        end;
        ftBlob:
          if fForceBlobAsNull then
            WR.AddShort('null') else begin
            if ColumnValueInlined then
              P := @V^.VData else
              P := V^.VAnsiChar;
            WR.WrBase64(P,V^.Length,true); // withMagic=true
          end;
        else WR.AddShort('null');
      end;
      stIsNull:
        WR.AddShort('null');
      stTruncated: begin
        LogTruncatedColumn(fColumns[col]);
        goto Write;
      end;
      else begin
        WR.AddShort('null');
        LogStatusError(V^.Status,@fColumns[col]);
      end;
    end;
    WR.Add(',');
  end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
end;

function TOleDBStatement.ParamToVariant(Param: Integer; var Value: Variant;
  CheckIsOutParameter: boolean): TSQLDBFieldType;
begin
  inherited ParamToVariant(Param,Value); // raise exception if Param incorrect
  dec(Param); // start at #1
  if CheckIsOutParameter and (fParams[Param].VInOut=paramIn) then
    raise EOleDBException.CreateUTF8('%.ParamToVariant expects an [In]Out parameter',[self]);
  // OleDB provider should have already modified the parameter in-place, i.e.
  // in our fParams[] buffer, especialy for TEXT parameters (OleStr/WideString)
  // -> we have nothing to do but return the current value :)
  with fParams[Param] do begin
    result := VType;
    case VType of
      ftInt64:     Value := {$ifdef DELPHI5OROLDER}integer{$endif}(VInt64);
      ftDouble:    Value := unaligned(PDouble(@VInt64)^);
      ftCurrency:  Value := PCurrency(@VInt64)^;
      ftDate:      Value := PDateTime(@VInt64)^;
      ftUTF8:      Value := VText; // returned as WideString/OleStr variant
      ftBlob:      RawByteStringToVariant(VBlob,Value);
      else         SetVariantNull(Value);
    end;
  end;
end;

const
  PARAMTYPE2OLEDB: array[TSQLDBParamInOutType] of DBPARAMIO = (
    DBPARAMIO_INPUT, DBPARAMIO_OUTPUT, DBPARAMIO_INPUT or DBPARAMIO_OUTPUT);
  FIELDTYPE2OLEDB: array[TSQLDBFieldType] of DBTYPE = (
    DBTYPE_EMPTY, DBTYPE_I4, DBTYPE_I8, DBTYPE_R8, DBTYPE_CY, DBTYPE_DATE,
    DBTYPE_WSTR or DBTYPE_BYREF, DBTYPE_BYTES or DBTYPE_BYREF);
  FIELDTYPE2OLEDBTYPE_NAME: array[TSQLDBFieldType] of WideString = (
    '', 'DBTYPE_I4', 'DBTYPE_I8', 'DBTYPE_R8', 'DBTYPE_CY', 'DBTYPE_DATE',
    'DBTYPE_WVARCHAR', 'DBTYPE_BINARY');
// ftUnknown, ftNull, ftInt64, ftDouble, ftCurrency, ftDate, ftUTF8, ftBlob
   TABLE_PARAM_DATASOURCE: WideString = 'table';
   //See BindArray
   IDList_type: WideString = 'IDList';
   StrList_TYPE: WideString = 'StrList';

procedure TOleDBStatement.Prepare(const aSQL: RawUTF8; ExpectResults: Boolean);
var L: integer;
    SQLW: RawUnicode;
begin
  SQLLogBegin(sllDB);
  if Assigned(fCommand) or Assigned(fRowSet) or (fColumnCount>0) or
     (fColumnBindings<>nil) or (fParamBindings<>nil) then
    raise EOleDBException.CreateUTF8('%.Prepare should be called once',[self]);
  inherited;
  with OleDBConnection do begin
    if not IsConnected then
      Connect;
    OleDBCheck(self,(fSession as IDBCreateCommand).
      CreateCommand(nil,IID_ICommandText,ICommand(fCommand)));
  end;
  L := Length(fSQL);
  if StripSemicolon then
    while (L>0) and (fSQL[L] in [#1..' ',';']) do
      dec(L); // trim ' ' or ';' right (last ';' could be found incorrect)
  SetLength(SQLW,L*2+1);
  UTF8ToWideChar(pointer(SQLW),pointer(fSQL),L);
  fCommand.SetCommandText(DBGUID_DEFAULT,pointer(SQLW));
  SQLLogEnd;
end;

procedure TOleDBStatement.ExecutePrepared;
var i: integer;
    P: POleDBStatementParam;
    B: PDBBinding;
    ParamsStatus: TCardinalDynArray;
    RowSet: IRowSet;
    mr: IMultipleResults;
    res: HResult;
    fParamBindInfo: TDBParamBindInfoDynArray;
    BI: PDBParamBindInfo;
    fParamOrdinals: TPtrUIntDynArray;
    PO: PPtrUInt;
    dbObjTVP: TDBObject;
    ssPropParamIDList: TDBPROP;
    ssPropsetParamIDList: TDBPROPSET;
    ssPropParamStrList: TDBPROP;
    ssPropsetParamStrList: TDBPROPSET;
    ssParamProps: TSSPARAMPROPSDynArray;
    ssParamPropsCount: integer;
    IDLists: array of TIDListRowset;
begin
  SQLLogBegin(sllSQL);
  // 1. check execution context
  if not Assigned(fCommand) then
    raise EOleDBException.CreateUTF8('%s.Prepare should have been called',[self]);
  if Assigned(fRowSet) or (fColumnCount>0) or
     (fColumnBindings<>nil) or (fParamBindings<>nil) then
    raise EOleDBException.CreateUTF8('Missing call to %.Reset',[self]);
  inherited ExecutePrepared; // set fConnection.fLastAccessTicks
  // 2. bind parameters
  SetLength(IDLists,fParamCount);
  try
    if fParamCount=0 then
      // no parameter to bind
      fDBParams.cParamSets := 0 else begin
      // bind supplied parameters, with direct mapping to fParams[]
      for i := 0 to fParamCount-1 do
        case fParams[i].VType of
          ftUnknown: raise EOleDBException.CreateUTF8(
            '%.Execute: missing #% bound parameter for [%]',[self,i+1,fSQL]);
        end;
      P := pointer(fParams);
      SetLength(fParamBindings,fParamCount);
      B := pointer(fParamBindings);
      SetLength(fParamBindInfo, fParamCount);
      BI := pointer(fParamBindInfo);
      SetLength(fParamOrdinals, fParamCount);
      PO := pointer(fParamOrdinals);
      dbObjTVP.dwFlags := STGM_READ;
      dbObjTVP.iid := IID_IRowset;
      FillChar(ssPropParamIDList,SizeOf(ssPropParamIDList),0);
      ssPropParamIDList.dwPropertyID := SSPROP_PARAM_TYPE_TYPENAME;
      ssPropParamIDList.vValue := IDList_TYPE;
      ssPropsetParamIDList.cProperties := 1;
      ssPropsetParamIDList.guidPropertySet := DBPROPSET_SQLSERVERPARAMETER;
      ssPropsetParamIDList.rgProperties := @ssPropParamIDList;
      FillChar(ssPropParamStrList,SizeOf(ssPropParamStrList),0);
      ssPropParamStrList.dwPropertyID := SSPROP_PARAM_TYPE_TYPENAME;
      ssPropParamStrList.vValue := StrList_TYPE;
      ssPropsetParamStrList.cProperties := 1;
      ssPropsetParamStrList.guidPropertySet := DBPROPSET_SQLSERVERPARAMETER;
      ssPropsetParamStrList.rgProperties := @ssPropParamStrList;
      SetLength(ssParamProps, fParamCount);
      ssParamPropsCount := 0;
      for i := 1 to fParamCount do begin
        B^.iOrdinal := i; // parameter index (starting at 1)
        B^.eParamIO := PARAMTYPE2OLEDB[P^.VInOut]; // parameter direction
        B^.wType := FIELDTYPE2OLEDB[P^.VType];     // parameter data type
        B^.dwPart := DBPART_VALUE or DBPART_STATUS;
        B^.obValue := PAnsiChar(@P^.VInt64)-pointer(fParams);
        B^.obStatus := PAnsiChar(@P^.VStatus)-pointer(fParams);
        BI^.dwFlags := PARAMTYPE2OLEDB[P^.VInOut]; // parameter direction
        BI^.pwszName := nil; //unnamed parameters
        BI^.pwszDataSourceType :=  Pointer(FIELDTYPE2OLEDBTYPE_NAME[P^.VType]);
        BI^.ulParamSize := 0;
        PO^ := i;
        // check array binding
        if P.VArray<>nil then begin
          BI^.pwszDataSourceType := Pointer(TABLE_PARAM_DATASOURCE);
          B^.wType := DBTYPE_TABLE;
          B^.cbMaxLen := sizeof(IUnknown);
          B^.pObject := @dbObjTVP;
          B^.obValue := PAnsiChar(@P^.VIUnknown)-pointer(fParams);
          case P^.VType of
            ftInt64: ssParamProps[ssParamPropsCount].rgPropertySets := @ssPropsetParamIDList;
            ftUTF8: ssParamProps[ssParamPropsCount].rgPropertySets := @ssPropsetParamStrList;
            else raise EOleDBException.Create('Unsupported array parameter type');
          end;
          ssParamProps[ssParamPropsCount].cPropertySets := 1;
          ssParamProps[ssParamPropsCount].iOrdinal := i;
          inc(ssParamPropsCount);
          IDLists[i-1] := TIDListRowset.Create(P.VArray, P^.VType);
          IDLists[i-1].Initialize(OleDBConnection.fSession as IOpenRowset);
          P^.VIUnknown := IDLists[i-1];
        end else begin
          P^.VIUnknown := nil;
          case P^.VType of
          ftNull: begin
            P^.VStatus := ord(stIsNull);
            BI.pwszDataSourceType := 'DBTYPE_WVARCHAR';
            BI.dwFlags := BI^.dwFlags or DBPARAMFLAGS_ISNULLABLE;
          end;
          ftInt64, ftDouble, ftCurrency, ftDate:
            // those types match the VInt64 binary representation :)
            B^.cbMaxLen := sizeof(Int64);
          ftBlob: begin
            // sent as DBTYPE_BYREF mapping directly RawByteString VBlob content
            B^.dwPart := DBPART_VALUE or DBPART_LENGTH or DBPART_STATUS;
            B^.obValue := PAnsiChar(@P^.VBlob)-pointer(fParams);
            B^.cbMaxLen := length(P^.VBlob);
            P^.VInt64 := length(P^.VBlob); // store length in unused VInt64 property
            B^.obLength := PAnsiChar(@P^.VInt64)-pointer(fParams);
          end;
          ftUTF8: begin
            B^.obValue := PAnsiChar(@P^.VText)-pointer(fParams);
            if P^.VText='' then begin
              B^.wType := DBTYPE_WSTR; // '' -> bind one #0 wide char
              B^.cbMaxLen := sizeof(WideChar);
            end else begin
              // mapping directly the WideString VText content
              B^.wType := DBTYPE_BSTR; // DBTYPE_WSTR just doesn't work :(
              B^.cbMaxLen := sizeof(Pointer);
              BI^.ulParamSize := length(P^.VText);
            end;
          end;
          end;
          if BI^.ulParamSize = 0 then
            BI^.ulParamSize := B^.cbMaxLen;
        end;
        inc(P);
        inc(B);
        inc(BI);
        inc(PO);
      end;
      if not OleDBConnection.OleDBProperties.fSupportsOnlyIRowset then begin
        OleDBConnection.OleDBCheck(self,
          (fCommand as ICommandWithParameters).SetParameterInfo(
            fParamCount, pointer(fParamOrdinals), pointer(fParamBindInfo)));
        if ssParamPropsCount>0 then
          OleDBConnection.OleDBCheck(self,
            (fCommand as ISSCommandWithParameters).SetParameterProperties(
              ssParamPropsCount, pointer(ssParamProps)));
      end;
      SetLength(ParamsStatus,fParamCount);
      OleDBConnection.OleDBCheck(self,
        (fCommand as IAccessor).CreateAccessor(
          DBACCESSOR_PARAMETERDATA,fParamCount,Pointer(fParamBindings),0,
          fDBParams.HACCESSOR,pointer(ParamsStatus)),ParamsStatus);
      fDBParams.cParamSets := 1;
      fDBParams.pData := pointer(fParams);
    end;
    // 3. Execute SQL
    if fExpectResults then
    try
      // 3.1 SELECT will allow access to resulting rows data from fRowSet
      res := E_UNEXPECTED; // makes compiler happy
      if not OleDBConnection.OleDBProperties.fSupportsOnlyIRowset then begin
        // use IMultipleResults for 'insert into table1 values (...); select ... from table2 where ...'
        res := fCommand.Execute(nil,IID_IMultipleResults,fDBParams,@fUpdateCount,@mr);
        if res=E_NOINTERFACE then
          OleDBConnection.OleDBProperties.fSupportsOnlyIRowset := true else
          if Assigned(mr) then
            repeat
              res := mr.GetResult(nil,0,IID_IRowset,@fUpdateCount,@RowSet);
            until Assigned(RowSet) or (res <> S_OK);
      end;
      if OleDBConnection.OleDBProperties.fSupportsOnlyIRowset then
        res := fCommand.Execute(nil,IID_IRowset,fDBParams,nil,@RowSet);
      OleDBConnection.OleDBCheck(self,res,ParamsStatus);
      FromRowSet(RowSet);
    except
      on E: Exception do begin
        CloseRowSet; // force fRowSet=nil
        raise;
      end;
    end else
      // 3.2 ExpectResults=false (e.g. SQL UPDATE) -> leave fRowSet=nil
      OleDBConnection.OleDBCheck(self,
        fCommand.Execute(nil,DB_NULLGUID,fDBParams,@fUpdateCount,nil));
  finally
    for i := 0 to fParamCount - 1 do
      if Assigned(IDLists[i]) then begin
        fParams[i].VIUnknown := nil;
        IDLists[i].Free;
      end;
  end;
  SQLLogEnd;
end;

procedure TOleDBStatement.FromRowSet(RowSet: IRowSet);
begin
  if fRowSet<>nil then
    EOleDBException.Create('TOleDBStatement.FromRowSet twice');
  if not Assigned(RowSet) then
    exit; // no row returned
  fRowSet := RowSet;
  fRowSize := BindColumns(fRowSet as IColumnsInfo,fColumn,fColumnBindings);
  SetLength(fRowSetData,fRowSize);
  if fRowSize>RowBufferSize then
    RowBufferSize := fRowSize; // enforce at least one row in OleDB buffer
  SetLength(fRowStepHandles,RowBufferSize div fRowSize);
end;

procedure TOleDBStatement.FlushRowSetData;
var c: integer;
begin
  if fHasColumnValueInlined then
    for c := 0 to fColumnCount-1 do
      with fColumns[c] do
      if not ColumnValueInlined then // release DBTYPE_BYREF memory
      with PColumnValue(@fRowSetData[ColumnAttr])^ do
        if VWideChar<>nil then
          OleDBConnection.fMalloc.Free(VWideChar);
  fillchar(fRowSetData[0],fRowSize,0);
end;

function TOleDBStatement.Step(SeekFirst: boolean): boolean;
var Status: TCardinalDynArray;
    sav: integer;
begin
{  if not Assigned(fCommand) then
    raise EOleDBException.CreateUTF8('%.Execute should be called before Step',[self]); }
  result := false;
  sav := fCurrentRow;
  fCurrentRow := 0;
  if not Assigned(fRowSet) or (fColumnCount=0) then
    exit; // no row available at all (e.g. for SQL UPDATE) -> return false
  if fRowSetAccessor=0 then begin
    // first time called -> need to init accessor from fColumnBindings[]
    SetLength(Status,fColumnCount);
    OleDBConnection.OleDBCheck(self,(fRowSet as IAccessor).CreateAccessor(
      DBACCESSOR_ROWDATA or DBACCESSOR_OPTIMIZED,fColumnCount,
      pointer(fColumnBindings),fRowSize,fRowSetAccessor,pointer(Status)),Status);
    fRowStepHandleRetrieved := 0;
    fRowStepHandleCurrent := 0;
    fRowStepResult := 0;
  end else
  if SeekFirst then begin
    // rewind to first row
    ReleaseRowSetDataAndRows;
    OleDBConnection.OleDBCheck(self,fRowSet.RestartPosition(DB_NULL_HCHAPTER));
    fRowStepResult := 0;
  end else
    FlushRowSetData;
  if fRowStepHandleCurrent>=fRowStepHandleRetrieved then begin
    ReleaseRowSetDataAndRows;
    if fRowStepResult=DB_S_ENDOFROWSET then
      exit; // no more row available -> return false
    fRowStepResult := fRowSet.GetNextRows(DB_NULL_HCHAPTER,0,length(fRowStepHandles),
      fRowStepHandleRetrieved,pointer(fRowStepHandles));
    OleDBConnection.OleDBCheck(self,fRowStepResult);
    fRowStepHandleCurrent := 0;
    if fRowStepHandleRetrieved=0 then
      exit; // no more row available
  end;
  // here we have always fRowStepHandleCurrent<fRowStepHandleRetrieved
  OleDBConnection.OleDBCheck(self,fRowSet.GetData(fRowStepHandles[fRowStepHandleCurrent],
    fRowSetAccessor,pointer(fRowSetData)));
  inc(fRowStepHandleCurrent);
  fCurrentRow := sav+1;
  inc(fTotalRowsRetrieved);
  result := true; // mark data available in fRowSetData
end;

destructor TOleDBStatement.Destroy;
begin
  try
    CloseRowSet;
  finally
    fCommand := nil;
    inherited;
  end;
end;

procedure TOleDBStatement.SetRowBufferSize(Value: integer);
begin
  if Value<4096 then
    Value := 4096;
  fRowBufferSize := Value;
end;

procedure TOleDBStatement.ReleaseRowSetDataAndRows;
begin
  FlushRowSetData;
  if fRowStepHandleRetrieved<>0 then begin
    fRowSet.ReleaseRows(fRowStepHandleRetrieved,Pointer(fRowStepHandles),nil,nil,nil);
    fRowStepHandleRetrieved := 0;
  end;
  fCurrentRow := 0;
end;

procedure TOleDBStatement.CloseRowSet;
begin
  if not Assigned(fRowSet) then
    exit;
  ReleaseRowSetDataAndRows;
  if fRowSetAccessor<>0 then begin
    (fRowSet as IAccessor).ReleaseAccessor(fRowSetAccessor,nil);
    fRowSetAccessor := 0;
  end;
  fRowSet := nil;
end;

procedure TOleDBStatement.Reset;
begin
  ReleaseRows;
  if fColumnCount>0 then begin
    fColumn.Clear;
    fColumn.ReHash;
    // faster if full command is re-prepared!
    fCommand := nil;
    Prepare(fSQL,fExpectResults);
  end;
  fUpdateCount := 0;
  inherited Reset;
end;

procedure TOleDBStatement.ReleaseRows;
begin
  if fParamCount>0 then
    fParam.Clear;
  fParamBindings := nil;
  CloseRowSet;
  fColumnBindings := nil;
  inherited ReleaseRows;
end;

function TOleDBStatement.UpdateCount: integer;
begin
  if not fExpectResults then
    result := fUpdateCount else
    result := 0;
end;

function OleDBColumnToFieldType(wType: DBTYPE; bScale: byte): TSQLDBFieldType;
begin
  case wType of
  DBTYPE_EMPTY:
    result := ftUnknown;
  DBTYPE_NULL:
    result := ftNull;
  DBTYPE_I1, DBTYPE_I2, DBTYPE_I4, DBTYPE_I8,
  DBTYPE_UI1, DBTYPE_UI2, DBTYPE_UI4, DBTYPE_UI8, DBTYPE_BOOL:
    result := ftInt64;
  DBTYPE_CY:
    result := ftCurrency;
  DBTYPE_R4, DBTYPE_R8:
    result := ftDouble;
  DBTYPE_DECIMAL, DBTYPE_NUMERIC, DBTYPE_VARNUMERIC:
    case bScale of // number of digits to the right of the decimal point
         0: result := ftInt64;
      1..4: result := ftCurrency;
    else    result := ftDouble;
    end;
  DBTYPE_DATE, DBTYPE_DBDATE, DBTYPE_DBTIME, DBTYPE_FILETIME, DBTYPE_DBTIMESTAMP:
    result := ftDate;
  DBTYPE_BYTES, DBTYPE_UDT:
    result := ftBlob;
  else // all other types will be converted to text
    result := ftUtf8;
  end;
end;


function TOleDBStatement.BindColumns(ColumnInfo: IColumnsInfo;
  var Column: TDynArrayHashed; out Bindings: TDBBindingDynArray): integer;
const
  // column content is inlined up to 4 KB, otherwise will be stored as DBTYPE_BYREF
  MAXCOLUMNSIZE = 4000;
var i, len: integer;
    B: PDBBinding;
    Cols, nfo: PDBColumnInfo;
    Col: PSQLDBColumnProperty;
    nCols: PtrUInt;
    ColsNames: PWideChar;
    aName: RawUTF8;
begin
  nCols := 0;
  Cols := nil;
  ColsNames := nil;
  OleDBConnection.OleDBCheck(self,ColumnInfo.GetColumnInfo(nCols,Cols,ColsNames));
  try
    nfo := Cols;
    SetLength(fColumnBindings,nCols);
    B := pointer(fColumnBindings);
    result := 0; // resulting buffer will map TColumnValue layout
    fColumn.Capacity := nCols;
    for i := 1 to nCols do begin
      if (nfo^.pwszName=nil) or (nfo^.pwszName^=#0) then
        aName := 'col_'+Int32ToUTF8(i) else
        aName := RawUnicodeToUtf8(nfo^.pwszName,StrLenW(nfo^.pwszName));
      Col := fColumn.AddAndMakeUniqueName(aName); // set ColumnName := aName
      Col^.ColumnType := OleDBColumnToFieldType(nfo^.wType,nfo^.bScale);
      Col^.ColumnNonNullable := nfo^.dwFlags and DBCOLUMNFLAGS_MAYBENULL=0;
      Col^.ColumnAttr := result; // offset of status[-length]-value in fRowSetData[]
      Col^.ColumnValueInlined := true;
      B^.iOrdinal := nfo^.iOrdinal;
      B^.eParamIO := DBPARAMIO_NOTPARAM;
      B^.obStatus := result;
      inc(result,sizeof(PtrInt)); // TColumnValue.Status
      B^.wType := FIELDTYPE2OLEDB[Col^.ColumnType];
      case Col^.ColumnType of
      ftInt64, ftDouble, ftCurrency, ftDate: begin
        inc(result,sizeof(PtrUInt)); // ignore TColumnValue.Length
        B^.dwPart := DBPART_STATUS or DBPART_VALUE;
        B^.obValue := result;
        B^.cbMaxLen := sizeof(Int64);
        inc(result,sizeof(Int64));
      end;
      ftUTF8, ftBlob: begin
        B^.dwPart := DBPART_STATUS or DBPART_VALUE or DBPART_LENGTH;
        B^.obLength := result; // need length field in fRowSetData[]
        inc(result,sizeof(PtrUInt)); // TColumnValue.Length
        B^.obValue := result;
        if nfo^.ulColumnSize<MAXCOLUMNSIZE then begin // inline up to 4 KB
          B^.wType :=  B^.wType and not DBTYPE_BYREF;
          Len := nfo^.ulColumnSize;
          Col^.ColumnValueDBSize := Len;
          if Col^.ColumnType=ftUTF8 then begin
            case nfo^.wType of
            DBTYPE_STR, DBTYPE_BSTR, DBTYPE_WSTR:
              Len := Len*2; // ulColumnSize = WideChar count
            DBTYPE_GUID: Len := 78;
            else Len := 62; // 31 widechars will fit any type converted
            end;
            inc(Len,2); // reserve memory for trailing WideChar(#0)
          end;
          if AlignDataInternalBuffer then // 8 bytes alignment
            Len := ((Len-1) shr 3+1)shl 3;
          inc(result,Len);
          B^.cbMaxLen := Len;
        end else begin // get huge content by pointer (includes DBTYPE_BYREF)
          fHasColumnValueInlined := true;
          Col^.ColumnValueInlined := false;
          B^.cbMaxLen := sizeof(Pointer); // value=pointer in fRowSetData[]
          if AlignDataInternalBuffer then
            inc(result,8) else
            inc(result,sizeof(Pointer));
        end;
      end;
      else raise EOleDBException.CreateUTF8(
        '%.Execute: wrong column [%] (%) for [%]',[self,aName,
          GetEnumName(TypeInfo(TSQLDBFieldType),ord(Col^.ColumnType))^,fSQL]);
      end;
      inc(nfo);
      inc(B);
      if AlignDataInternalBuffer then
        Assert((result and 7)=0);
    end;
    assert(not AlignDataInternalBuffer or (result and 7=0));
    assert(fColumnCount=integer(nCols));
  finally
    OleDBConnection.fMalloc.Free(Cols);
    OleDBConnection.fMalloc.Free(ColsNames);
  end;
end;


{ TOleDBConnection }

threadvar
  OleDBCoinitialized: integer;

procedure CoInit;
begin
  inc(OleDBCoInitialized);
  if OleDBCoInitialized=1 then
    CoInitialize(nil);
end;

procedure CoUninit;
begin
  assert(OleDBCoinitialized>0,'You should call TOleDBConnection.Free from the same '+
    'thread which called its Create: i.e. call MyProps.EndCurrentThread from an '+
    'THttpServerGeneric.OnHttpThreadTerminate event - see ticket 213544b2f5');
  dec(OleDBCoinitialized);
  if OleDBCoinitialized=0 then
    CoUninitialize;
end;

procedure TOleDBConnection.Connect;
var DataInitialize : IDataInitialize;
    unknown: IUnknown;
    Log: ISynLog;
begin
  Log := SynDBLog.Enter(self,'Connect');
  // check context
  if Connected then
    Disconnect;
  if OleDBProperties.ConnectionString='' then
    raise EOleDBException.CreateUTF8('%.Connect excepts a ConnectionString',[self]);
  try
    // retrieve initialization parameters from connection string
    OleCheck(CoCreateInstance(CLSID_MSDAINITIALIZE, nil, CLSCTX_INPROC_SERVER,
      IID_IDataInitialize, DataInitialize));
    OleCheck(DataInitialize.GetDataSource(nil,CLSCTX_INPROC_SERVER,
      pointer(OleDBProperties.ConnectionString),
      IID_IDBInitialize,IUnknown(fDBInitialize)));
    DataInitialize := nil;
    // open the connection to the DB
    OleDBCheck(nil,fDBInitialize.Initialize);
    OnDBInitialized; // optionaly set parameters
    OleDBCheck(nil,
      (fDBInitialize as IDBCreateSession).CreateSession(nil,IID_IOpenRowset,fSession));
    // check if DB handle transactions
    if fSession.QueryInterface(IID_ITransactionLocal,unknown)=S_OK then
      fTransaction := unknown as ITransactionLocal else
      fTransaction := nil;
    inherited Connect; // notify any re-connection
  except
    on E: Exception do begin
      fSession := nil; // mark not connected
      fDBInitialize := nil;
      DataInitialize := nil;
      raise;
    end;
  end;
end;

constructor TOleDBConnection.Create(aProperties: TSQLDBConnectionProperties);
var Log: ISynLog;
begin
  Log := SynDBLog.Enter(self,'Create');
  if not aProperties.InheritsFrom(TOleDBConnectionProperties) then
    raise EOleDBException.CreateUTF8('Invalid %.Create(%)',[self,aProperties]);
  fOleDBProperties := TOleDBConnectionProperties(aProperties);
  inherited;
  CoInit;
  OleCheck(CoGetMalloc(1,fMalloc));
end;

destructor TOleDBConnection.Destroy;
var Log: ISynLog;
begin
  Log := SynDBLog.Enter(self,'Destroy');
  try
    inherited Destroy; // call Disconnect;
    fMalloc := nil;
    CoUninit;
  except
    on E: Exception do
      if Log<>nil then
        Log.Log(sllError,E);
  end;
end;

procedure TOleDBConnection.Disconnect;
var Log: ISynLog;
begin
  Log := SynDBLog.Enter(self,'Disconnect');
  try
    inherited Disconnect; // flush any cached statement
  finally
    if Connected then begin
      fTransaction := nil;
      fSession := nil;
      OleDBCheck(nil,fDBInitialize.Uninitialize);
      fDBInitialize := nil;
    end;
  end;
end;

function TOleDBConnection.IsConnected: boolean;
begin
  result := fSession<>nil;
end;

function TOleDBConnection.NewStatement: TSQLDBStatement;
begin
  result := TOleDBStatement.Create(self);
end;

procedure TOleDBConnection.OleDBCheck(aStmt: TSQLDBStatement; aResult: HRESULT;
  const aStatus: TCardinalDynArray);
procedure EnhancedTest;
var ErrorInfo, ErrorInfoDetails: IErrorInfo;
    ErrorRecords: IErrorRecords;
    i: integer;
    Desc: WideString;
    ErrorCount: UINT;
    E: Exception;
    s: string;
begin // get OleDB specific error information
  GetErrorInfo(0,ErrorInfo);
  if Assigned(ErrorInfo) then begin
    ErrorRecords := ErrorInfo as IErrorRecords;
    ErrorRecords.GetRecordCount(ErrorCount);
    for i := 0 to ErrorCount-1 do
      if not Assigned(OleDBProperties.OnCustomError) or
         not OleDBProperties.OnCustomError(self,ErrorRecords,i) then begin
        // retrieve generic error info if OnCustomError() didn't handle it
        OleCheck(ErrorRecords.GetErrorInfo(i,GetSystemDefaultLCID,ErrorInfoDetails));
        OleCheck(ErrorInfoDetails.GetDescription(Desc));
        if fOleDBErrorMessage<>'' then
          fOleDBErrorMessage := fOleDBErrorMessage+'  ';
        fOleDBErrorMessage := fOleDBErrorMessage+UnicodeBufferToString(pointer(Desc));
        ErrorInfoDetails := nil;
      end;
  end;
  // get generic HRESULT error
  if not Succeeded(aResult) or (fOleDBErrorMessage<>'') then begin
    s := SysErrorMessage(aResult);
    if s='' then
      s := 'OLEDB Error '+IntToHex(aResult,8);
    if s<>fOleDBErrorMessage then
      fOleDBErrorMessage := s+' - '+fOleDBErrorMessage;
  end;
  if fOleDBErrorMessage='' then
    exit;
  // retrieve binding information from Status[]
  s := '';
  for i := 0 to high(aStatus) do
    if TOleDBBindStatus(aStatus[i])<>bsOK then begin
      if aStatus[i]<=cardinal(high(TOleDBBindStatus)) then
        s := FormatString('% Status[%]="%"',[s,i,
          GetCaptionFromEnum(TypeInfo(TOleDBBindStatus),aStatus[i])]) else
        s := FormatString('% Status[%]=%',[s,i,aStatus[i]]);

    end;
  if s<>'' then
    fOleDBErrorMessage :=  fOleDBErrorMessage+s;
  StringToUTF8(fOleDBErrorMessage, fErrorMessage);
  // raise exception
  if aStmt=nil then
    E := EOleDBException.Create(fOleDBErrorMessage) else
    E := EOleDBException.CreateUTF8('%: %',[self,StringToUTF8(fOleDBErrorMessage)]);
  SynDBLog.Add.Log(sllError,E);
  raise E;
end;
begin
  fOleDBErrorMessage := '';
  fOleDBInfoMessage := '';
  if not Succeeded(aResult) or Assigned(OleDBProperties.OnCustomError) then
    EnhancedTest;
end;

procedure TOleDBConnection.OnDBInitialized;
begin // do nothing by default
end;

procedure TOleDBConnection.Commit;
var Log: ISynLog;
begin
  Log := SynDBLog.Enter(self,'Commit');
  if assigned(fTransaction) then begin
    inherited Commit;
    try
      OleDbCheck(nil,fTransaction.Commit(False,XACTTC_SYNC,0));
    except
      inc(fTransactionCount); // the transaction is still active
      raise;
    end;
  end;
end;

procedure TOleDBConnection.Rollback;
var Log: ISynLog;
begin
  Log := SynDBLog.Enter(self,'Rollback');
  if assigned(fTransaction) then begin
    inherited Rollback;
    OleDbCheck(nil,fTransaction.Abort(nil,False,False));
  end;
end;

procedure TOleDBConnection.StartTransaction;
var Log: ISynLog;
begin
  Log := SynDBLog.Enter(self,'StartTransaction');
  if assigned(fTransaction) then begin
    inherited StartTransaction;
    OleDbCheck(nil,fTransaction.StartTransaction(ISOLATIONLEVEL_READCOMMITTED,0,nil,nil));
  end;
end;


{ TOleDBConnectionProperties }

function TOleDBConnectionProperties.ConnectionStringDialogExecute(Parent: HWND): boolean;
var DataInitialize: IDataInitialize;
    DBPromptInitialize: IDBPromptInitialize;
    DBInitialize: IUnknown;
    res: HRESULT;
    tmp: PWideChar;
    Log: ISynLog;
begin
  Log := SynDBLog.Enter(self,'ConnectionStringDialog');
  result := false;
  if self<>nil then
  try
    CoInit; // if not already done
    try
      OleCheck(CoCreateInstance(CLSID_DATALINKS, nil, CLSCTX_INPROC_SERVER,
        IID_IDBPromptInitialize, DBPromptInitialize));
      OleCheck(CoCreateInstance(CLSID_MSDAINITIALIZE, nil, CLSCTX_INPROC_SERVER,
        IID_IDataInitialize, DataInitialize));
      if fConnectionString<>'' then
        DataInitialize.GetDataSource(nil,CLSCTX_INPROC_SERVER,Pointer(fConnectionString),
          IID_IDBInitialize,DBInitialize) else
        DBInitialize := nil;
      res := DBPromptInitialize.PromptDataSource(nil,Parent,DBPROMPTOPTIONS_PROPERTYSHEET,
        0,nil,nil,IID_IDBInitialize,DBInitialize);
      case res of
      S_OK: begin
        OleCheck(DataInitialize.GetInitializationString(DBInitialize,True,tmp));
        fConnectionString := tmp;
        if tmp<>nil then
          CoTaskMemFree(tmp);
        if Log<>nil then
          Log.Log(sllDB,'New connection settings set',self);
        result := true;
      end;
      DB_E_CANCELED:
        if Log<>nil then
          Log.Log(sllDB,'Canceled',self);
      else OleCheck(res);
      end;
    finally
      CoUninit;
    end;
  except
    result  := false;
  end;
end;

const
  CLASS_Catalog: TGUID = '{00000602-0000-0010-8000-00AA006D2EA4}';
  IID__Catalog: TGUID = '{00000603-0000-0010-8000-00AA006D2EA4}';

type
  _Catalog = interface(IDispatch)
    ['{00000603-0000-0010-8000-00AA006D2EA4}']
    function Get_Tables: OleVariant; safecall;
    function Get_ActiveConnection: OleVariant; safecall;
    procedure Set_ActiveConnection(pVal: OleVariant); safecall;
    procedure _Set_ActiveConnection(const pVal: IDispatch); safecall;
    function Get_Procedures: OleVariant; safecall;
    function Get_Views: OleVariant; safecall;
    function Get_Groups: OleVariant; safecall;
    function Get_Users: OleVariant; safecall;
    function Create(const ConnectString: WideString): OleVariant; safecall;
    // warning: the following method won't work if you use SynFastWideString.pas
    // but we don't call it in this unit, you we can stay cool for now :)
    function GetObjectOwner(const ObjectName: WideString; ObjectType: OleVariant;
                            ObjectTypeId: OleVariant): WideString; safecall;
    procedure SetObjectOwner(const ObjectName: WideString; ObjectType: OleVariant;
                             const UserName: WideString; ObjectTypeId: OleVariant); safecall;
  end;

function TOleDBConnectionProperties.CreateDatabase: boolean;
var Catalog: _Catalog;
    DB: OleVariant;
begin
  result := false;
  if ConnectionString<>'' then
  try
    CoInit;
    if Succeeded(CoCreateInstance(CLASS_Catalog, nil, CLSCTX_INPROC_SERVER,
      IID__Catalog, Catalog)) then
      try
        DB := Catalog.Create(ConnectionString);
        result := true;
      except
        result := false;
      end;
    SynDBLog.Add.Log(sllDB,'CreateDatabase for [%] returned %',
      [ConnectionString,ord(result)],self);
  finally
    DB := null;
    Catalog := nil;
    CoUninit;
  end;
end;

procedure TOleDBConnectionProperties.GetTableNames(out Tables: TRawUTF8DynArray);
var Rows: IRowset;
    count, schemaCol, nameCol: integer;
    schema, tablename: RawUTF8;
begin
  inherited; // first try from SQL, if any (faster)
  if Tables<>nil then
    exit; // already retrieved directly from engine
  try
    // see http://msdn.microsoft.com/en-us/library/ms716980(v=VS.85).aspx
    // Restriction columns: TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, TABLE_TYPE
    if GetSchema(DBSCHEMA_TABLES,['','','','TABLE'],Rows) then
    with TOleDBStatement.Create(MainConnection) do
    try
      FromRowSet(Rows);
      count := 0;
      schemaCol := ColumnIndex('TABLE_SCHEMA');
      nameCol := ColumnIndex('TABLE_NAME');
      if (schemaCol>=0) and (nameCol>=0) then
        while Step do begin
          schema := Trim(ColumnUTF8(schemaCol));
          tablename := Trim(ColumnUTF8(nameCol));
          if schema<>'' then
            tablename := schema+'.'+tablename;
          AddSortedRawUTF8(Tables,count,tableName);
        end;
      SetLength(Tables,count);
    finally
      Free;
    end;
  except
    on Exception do
      SetLength(Tables,0);
  end;
end;

procedure TOleDBConnectionProperties.GetFields(const aTableName: RawUTF8;
  out Fields: TSQLDBColumnDefineDynArray);
var Owner, Table, Column: RawUTF8;
    Rows: IRowset;
    n, i: integer;
    F: TSQLDBColumnDefine;
    FA: TDynArray;
const DBTYPE_DISPLAY: array[TSQLDBFieldType] of RawUTF8 = (
  '???','null','int','double','currency','date','nvarchar','blob');
begin
  inherited; // first try from SQL, if any (faster)
  if Fields<>nil then
    exit; // already retrieved directly from engine
  try
    Split(aTableName,'.',Owner,Table);
    if Table='' then begin
      Table := Owner;
      Owner := '';
    end;
    // see http://msdn.microsoft.com/en-us/library/ms723052(v=VS.85).aspx
    if GetSchema(DBSCHEMA_COLUMNS,['',Owner,Table,''],Rows) then
      // Restriction columns: TABLE_CATALOG,TABLE_SCHEMA,TABLE_NAME,COLUMN_NAME
      with TOleDBStatement.Create(MainConnection) do
      try
        FromRowSet(Rows);
        FA.Init(TypeInfo(TSQLDBColumnDefineDynArray),Fields,@n);
        while Step do begin
          F.ColumnName := Trim(ColumnUTF8('COLUMN_NAME'));
          F.ColumnLength := ColumnInt('CHARACTER_MAXIMUM_LENGTH');
          F.ColumnPrecision := ColumnInt('NUMERIC_PRECISION');
          F.ColumnScale := ColumnInt('NUMERIC_SCALE');
          F.ColumnType:= OleDBColumnToFieldType(ColumnInt('DATA_TYPE'),F.ColumnScale);
          F.ColumnTypeNative := DBTYPE_DISPLAY[F.ColumnType];
          FA.Add(F);
        end;
        SetLength(Fields,n);
      finally
        Free;
      end;
  // now we have Fields[] with the column information -> get indexes and foreign keys
  if GetSchema(DBSCHEMA_INDEXES,['',Owner,'','',Table],Rows) then
    // Restriction columns: TABLE_CATALOG,TABLE_SCHEMA,INDEX_NAME,TYPE,TABLE_NAME
    with TOleDBStatement.Create(MainConnection) do
    try
      FromRowSet(Rows);
      while Step do begin
        Column := Trim(ColumnUTF8('COLUMN_NAME'));
        for i := 0 to high(Fields) do
        with Fields[i] do
        if IdemPropNameU(ColumnName,Column) then begin
          ColumnIndexed := true;
          break;
        end;
      end;
    finally
      Free;
    end;
  except
    on Exception do
      SetLength(Fields,0);
  end;
end;

procedure TOleDBConnectionProperties.GetForeignKeys;
var Rows: IRowset;
begin // retrieve all foreign keys into fForeignKeys list
  try
    if GetSchema(DBSCHEMA_FOREIGN_KEYS,['','','','','',''],Rows) then
    // PK_TABLE_CATALOG,PK_TABLE_SCHEMA,PK_TABLE_NAME,FK_TABLE_CATALOG,FK_TABLE_SCHEMA,FK_TABLE_NAME
    with TOleDBStatement.Create(MainConnection) do
    try
      FromRowSet(Rows);
      while Step do
        fForeignKeys.Add(
          Trim(ColumnUTF8('FK_TABLE_SCHEMA'))+'.'+Trim(ColumnUTF8('FK_TABLE_NAME'))+
            '.'+Trim(ColumnUTF8('FK_COLUMN_NAME')),
          Trim(ColumnUTF8('PK_TABLE_SCHEMA'))+'.'+Trim(ColumnUTF8('PK_TABLE_NAME'))+
            '.'+Trim(ColumnUTF8('PK_COLUMN_NAME')));
    finally
      Free;
    end;
  except
    on Exception do ; // just ignore errors here
  end;
end;

function TOleDBConnectionProperties.GetSchema(const aUID: TGUID;
  const Fields: array of RawUTF8; var aResult: IRowset): boolean;
var i, res, n: integer;
    C: TOleDBConnection;
    SRS: IDBSchemaRowset;
    PG, OG: PGUID;
    PI, OI: PInteger;
    Args: array of Variant;
begin
  result := false;
  if (self=nil) or (high(Fields)<0) then
    exit;
  C := MainConnection as TOleDBConnection;
  if C.fSession=nil then
    C.Connect;
  C.fSession.QueryInterface(IDBSchemaRowset,SRS);
  if not Assigned(SRS) then
    exit; // provider do not support this interface
  if fSchemaRec=nil then begin
    SRS.GetSchemas(n,OG,OI);
    if n>0 then
    try
      SetLength(fSchemaRec,n);
      PG := OG;
      PI := OI;
      for i := 0 to n-1 do
      with fSchemaRec[i] do begin
        SchemaGuid := PG^;
        SupportedRestrictions := PI^;
        inc(PG);
        inc(PI);
      end;
    finally
      C.fMalloc.Free(OG);
      C.fMalloc.Free(OI);
    end;
  end;
  res := 0;
  for i := 0 to high(fSchemaRec) do
    if IsEqualGuid(@fSchemaRec[i].SchemaGuid,@aUID) then begin
      res := fSchemaRec[i].SupportedRestrictions;
      break;
    end;
  if res=0 then
    exit;
  SetLength(Args,length(Fields));
  for i := 0 to high(Fields) do
  if res and (1 shl i)<>0 then
    if Fields[i]<>'' then // '' will leave VT_EMPTY parameter = no restriction
      Args[i] := UTF8ToWideString(Fields[i]); // expect parameter as BSTR
  aResult := nil;
  try
    C.OleDBCheck(nil,SRS.GetRowset(nil,aUID,length(Args),Args,IID_IRowset,0,nil,aResult));
    result := aResult<>nil; // mark some rows retrieved
  except
    result := false;
  end;
end;

function TOleDBConnectionProperties.NewConnection: TSQLDBConnection;
begin
  result := TOleDBConnection.Create(self);
end;

procedure TOleDBConnectionProperties.SetInternalProperties;
var tmp: RawUTF8;
begin
  if fProviderName<>'' then
    tmp := 'Provider='+fProviderName+';';
  if fServerName<>'' then
    tmp := tmp+'Data Source='+fServerName+';';
  if fDatabaseName<>'' then
    tmp := tmp+'Initial Catalog='+fDatabaseName+';';
  fConnectionString := UTF8ToSynUnicode(tmp+'User Id='+fUserID+';Password='+fPassWord+';');
end;

function TOleDBConnectionProperties.ColumnTypeNativeToDB(
  const aNativeType: RawUTF8; aScale: integer): TSQLDBFieldType;
var native, err: integer;
begin
  native := GetInteger(pointer(aNativeType),err);
  if err=0 then
    // type directly retrieved from OleDB as integer
    result := OleDBColumnToFieldType(native,aScale) else
    // type retrieved via a SELECT from INFORMATION_SCHEMA.COLUMNS
    result := inherited ColumnTypeNativeToDB(aNativeType,aScale);
end;


{ TOleDBOracleConnectionProperties }

procedure TOleDBOracleConnectionProperties.SetInternalProperties;
begin
  if fProviderName='' then
    fProviderName := 'OraOLEDB.Oracle.1';
  fDBMS := dOracle;
  inherited SetInternalProperties;
end;


{ TOleDBMSOracleConnectionProperties }

procedure TOleDBMSOracleConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'MSDAORA';
  fDBMS := dOracle;
  inherited SetInternalProperties;
end;

{ TOleDBMSSQLConnectionProperties }

type
  /// to retrieve enhanced Microsoft SQL Server error information
  PSSERRORINFO = ^SSERRORINFO;
  SSERRORINFO = packed record
    pwszMessage: PWideChar;
    pwszServer: PWideChar;
    pwszProcedure: PWideChar;
    lNative: cardinal;
    bState: byte;
    bClass: byte;
    wLineNumber: word;
  end;
  /// to retrieve enhanced Microsoft SQL Server error information
  ISQLServerErrorInfo = interface(IUnknown)
    ['{5CF4CA12-EF21-11d0-97E7-00C04FC2AD98}']
    function GetErrorInfo(out ppErrorInfo: PSSERRORINFO;
                          out Error: PWideChar): HResult; stdcall;
  end;

const
  IID_ISQLServerErrorInfo: TGUID = '{5CF4CA12-EF21-11d0-97E7-00C04FC2AD98}';

function TOleDBMSSQLConnectionProperties.MSOnCustomError(Connection: TOleDBConnection;
  ErrorRecords: IErrorRecords; RecordNum: UINT): boolean;
var SQLServerErrorInfo: ISQLServerErrorInfo;
    SSErrorInfo: PSSERRORINFO;
    SSErrorMsg: PWideChar;
    msg, tmp: string;
    utf8: RawUTF8;
begin
  result := False;
  if (self=nil) or (Connection=nil) then
    exit;
  ErrorRecords.GetCustomErrorObject(RecordNum,IID_ISQLServerErrorInfo,
    IUnknown(SQLServerErrorInfo));
  if Assigned(SQLServerErrorInfo) then
  try
    if (SQLServerErrorInfo.GetErrorInfo(SSErrorInfo,SSErrorMsg)=S_OK) and
       (SSErrorInfo<>nil) then
    with SSErrorInfo^ do
    try
      msg := UnicodeBufferToString(pwszMessage)+#13#10;
      if bClass<=10 then begin
        Connection.fOleDBInfoMessage := Connection.fOleDBInfoMessage+msg;
        RawUnicodeToUtf8(pwszMessage,StrLenW(pwszMessage),utf8);
        SynDBLog.Add.Log(sllDB,utf8,self);
        with Connection.Properties do
          if Assigned(OnStatementInfo) then
            OnStatementInfo(nil,utf8);
      end else begin
        if pwszProcedure<>nil then
          tmp := UnicodeBufferToString(pwszProcedure) else
          tmp := 'Error '+IntToStr(lNative);
        Connection.fOleDBErrorMessage := FormatString('% % (line %): %',
          [Connection.fOleDBErrorMessage,tmp,wLineNumber,msg]);
      end;
    finally
      Connection.fMalloc.Free(SSErrorInfo);
      Connection.fMalloc.Free(SSErrorMsg);
    end;
    result := true;
  finally
    SQLServerErrorInfo := nil;
  end;
end;

procedure TOleDBMSSQLConnectionProperties.SetInternalProperties;
begin
  OnCustomError := MSOnCustomError;
  if fProviderName='' then
    fProviderName := 'SQLNCLI10';
  fDBMS := dMSSQL;
  inherited SetInternalProperties;
  if fUserID='' then
    fConnectionString := fConnectionString+
      'Integrated Security=SSPI;Persist Security Info=False;';
end;

{ TOleDBMSSQL2005ConnectionProperties }

procedure TOleDBMSSQL2005ConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'SQLNCLI';
  inherited SetInternalProperties;
end;

constructor TOleDBMSSQL2005ConnectionProperties.Create(
  const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  inherited;
  fBatchSendingAbilities := [];
  fOnBatchInsert := nil; // MultipleValuesInsert() does not work with SQL 2005
end;

{ TOleDBMSSQL2012ConnectionProperties }

procedure TOleDBMSSQL2012ConnectionProperties.SetInternalProperties;
begin
  if OSVersion>wVista then
    fProviderName := 'SQLNCLI11';
  inherited SetInternalProperties;
end;


{ TOleDBODBCSQLConnectionProperties }

constructor TOleDBODBCSQLConnectionProperties.Create(const aDriver,
  aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  fDriver := aDriver;
  inherited Create(aServerName,aDatabaseName,aUserID,aPassWord);
end;

procedure TOleDBODBCSQLConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'MSDASQL'; // we could have left it void - never mind
  inherited SetInternalProperties;
  if fDriver<>'' then
    fConnectionString := UTF8ToSynUnicode('Driver='+fDriver+';')+fConnectionString;
end;

{ TOleDBMySQLConnectionProperties }

procedure TOleDBMySQLConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'MYSQLPROV';
  fDBMS := dMySQL;
  inherited;
end;

{ TOleDBAS400ConnectionProperties }

procedure TOleDBAS400ConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'IBMDA400.DataSource.1';
  inherited SetInternalProperties;
end;

{ TOleDBInformixConnectionProperties }

procedure TOleDBInformixConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'Ifxoledbc';
  fDBMS := dInformix;
  inherited SetInternalProperties;
end;

{$ifndef CPU64}

{ TOleDBJetConnectionProperties }

procedure TOleDBJetConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'Microsoft.Jet.OLEDB.4.0';
  fDBMS := dJet;
  inherited SetInternalProperties;
  if not FileExists(UTF8ToString(ServerName)) then
    CreateDatabase;
end;

{$endif CPU64}

{ TOleDBACEConnectionProperties }

procedure TOleDBACEConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'Microsoft.ACE.OLEDB.12.0';
  fDBMS := dJet;
  inherited SetInternalProperties;
  if not FileExists(UTF8ToString(ServerName)) then
    CreateDatabase;
end;


{ TBaseAggregatingRowset }

function TBaseAggregatingRowset.AddRefRows(cRows: PtrUInt;
  rghRows: PPtrUIntArray; rgRefCounts, rgRowStatus: PCardinalArray): HResult;
begin
  // Never gets called, so we can return E_NOTIMPL
  Result := E_NOTIMPL;
end;

constructor TBaseAggregatingRowset.Create(cTotalRows: UINT);
begin
  fidxRow := 1;
  fcTotalRows := cTotalRows;
  fUnkInnerSQLNCLIRowset := nil;
  SetLength(fhAccessor,1);
  fhAccessor[0] := 0;
  inherited Create;
end;

destructor TBaseAggregatingRowset.Destroy;
var pIAccessor: IAccessor;
begin
  if (fhAccessor[0]<>0) then begin
    pIAccessor := nil;
    OleCheck(fUnkInnerSQLNCLIRowset.QueryInterface(IID_IAccessor, pIAccessor));
    OleCheck(pIAccessor.ReleaseAccessor(fhAccessor[0], nil));
  end;
  inherited;
end;

function TBaseAggregatingRowset.GetData(HROW: HROW; HACCESSOR: HACCESSOR;
  pData: Pointer): HResult;
begin
  Result := S_OK;
end;

function TBaseAggregatingRowset.GetNextRows(hReserved: HCHAPTER; lRowsOffset,
  cRows: PtrInt; out pcRowsObtained: PtrUInt; var prghRows: pointer): HResult;
begin
  assert(lRowsOffset = 0);
  assert(cRows = 1);
  assert(Assigned(prghRows));
  pcRowsObtained := 0;
  // If we still have rows to give back
  if (fidxRow <= fcTotalRows) then begin
     pcRowsObtained := 1;
     // For us, row handle is simply an index in our row list
     PHROW(prghRows)^ := fidxRow;
     Inc(fidxRow);
     Result := S_OK;
  end else
     Result := DB_S_ENDOFROWSET;
end;

{$ifdef FPC}
function TBaseAggregatingRowset.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;
  {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
{$else}
function TBaseAggregatingRowset.QueryInterface(const IID: TGUID; out Obj): HResult;
{$endif FPC}
begin
  if IsEqualGUID(@IID, @IID_IUnknown) then begin
    IUnknown(Obj) := Self;
  end else begin
    if not Assigned(fUnkInnerSQLNCLIRowset) then begin
      Pointer(Obj) := nil;
      Result := E_NOINTERFACE;
      Exit;
    end;
    if IsEqualGUID(@IID, @IID_IRowset)then begin
      IUnknown(Obj) := self;
    end else begin
      Result := fUnkInnerSQLNCLIRowset.QueryInterface(IID, Obj);
      exit;
    end;
  end;
  IUnknown(Obj)._AddRef;
  Result := S_OK;
end;

function TBaseAggregatingRowset.ReleaseRows(cRows: UINT; rghRows: PPtrUIntArray;
  rgRowOptions, rgRefCounts, rgRowStatus: PCardinalArray): HResult;
begin
  assert(cRows = 1);
  assert(rghRows[0] <= fcTotalRows);
  Result := S_OK;
end;

function TBaseAggregatingRowset.RestartPosition(hReserved: HCHAPTER): HResult;
begin
  fidxRow := 1;
  Result := S_OK;
end;

procedure TBaseAggregatingRowset.SetAccessorHandle(idxAccessor: ULONG;
  hAccessor: HACCESSOR);
begin
  fhAccessor[idxAccessor] := hAccessor;
end;

{$ifdef FPC}
function TBaseAggregatingRowset._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
{$else}
function TBaseAggregatingRowset._AddRef: Integer;
{$endif FPC}
begin
  Result := 1;
end;

{$ifdef FPC}
function TBaseAggregatingRowset._Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
{$else}
function TBaseAggregatingRowset._Release: Integer;
{$endif FPC}
begin
  Result := 1;
end;

{ TIDListRowset }

constructor TIDListRowset.Create(arr: TRawUTF8DynArray; aType: TSQLDBFieldType);
begin
  farr := arr;
  fType := aType;
  inherited Create(Length(farr ));
end;

procedure TIDListRowset.FillBindingsAndSetupRowBuffer(
  pBindingsList: PDBBindingArray);
var i: Integer;
    rec: TIDListRec; // pseudo record to compute offset within TIDListRec
begin
  fillchar(rec,sizeof(rec),0); // makes Win64 compiler happy
  pBindingsList[0].pTypeInfo := nil;
  pBindingsList[0].pObject := nil;
  pBindingsList[0].pBindExt := nil;
  pBindingsList[0].eParamIO := DBPARAMIO_NOTPARAM;
  pBindingsList[0].iOrdinal := 1;
  pBindingsList[0].dwPart := DBPART_VALUE or DBPART_STATUS or DBPART_LENGTH;
  pBindingsList[0].dwMemOwner := DBMEMOWNER_CLIENTOWNED;
  pBindingsList[0].dwFlags := 0;
  case fType of
    ftInt64: begin
      pBindingsList[0].cbMaxLen := sizeof(int64);
      pBindingsList[0].obValue := PAnsiChar(@rec.IDVal)-pointer(@rec);
      pBindingsList[0].wType := DBTYPE_I8;
    end;
    ftUTF8: begin
      pBindingsList[0].cbMaxLen := sizeof(PWideChar); //Check bind ''
      for I := 0 to Length(farr)-1 do
        if Length(farr[i])*SizeOf(WideChar)>Integer(pBindingsList[0].cbMaxLen) then
          pBindingsList[0].cbMaxLen := Length(farr[i])*SizeOf(WideChar);
      pBindingsList[0].obValue := PAnsiChar(@rec.StrVal)-pointer(@rec);
      pBindingsList[0].wType := DBTYPE_BSTR
    end;
  end;
  pBindingsList[0].obStatus := PAnsiChar(@rec.IDST)-pointer(@rec);
  pBindingsList[0].obLength := PAnsiChar(@rec.IDLen)-pointer(@rec);
end;

procedure TIDListRowset.FillRowData(pCurrentRec: PIDListRec);
var curInd: Integer;
    tmp: RawUTF8;
begin
  curInd := fidxRow-2;
  if farr[curInd]='null' then begin
    pCurrentRec.IDST := ord(stIsNull);
  end else begin
    pCurrentRec.IDST := 0;
    case fType of
      ftInt64: begin
        SetInt64(pointer(farr[curInd]),pCurrentRec.IDVal);
        pCurrentRec.IDLen := SizeOf(Int64);
      end;
      ftUTF8: begin
        tmp := UnQuoteSQLString(farr[curInd]);
        pCurrentRec.IDLen := (Length(tmp)+1)*SizeOf(WideChar);
        pCurrentRec.StrVal := Pointer(UTF8ToWideString(tmp));
      end
      else raise EOleDBException.Create('Unsupported array parameter type');
    end;
  end;
end;

function TIDListRowset.GetData(HROW: HROW; HACCESSOR: HACCESSOR;
  pData: Pointer): HResult;
var currentRec: PIDListRec;
begin
  inherited GetData(HROW, HACCESSOR, pData);
  currentRec := pData;
  FillRowData(currentRec);
  Result := S_OK;
end;

function TIDListRowset.Initialize(pIOpenRowset: IOpenRowset): HRESULT;
var dbidID: DBID;
begin
  dbidID.eKind := DBKIND_GUID_NAME;
  dbidID.uGuid.guid := CLSID_ROWSET_TVP;
  case fType of
    ftInt64: dbidID.uName.pwszName := pointer(IDList_type);
    ftUTF8:  dbidID.uName.pwszName := pointer(StrList_type);
  end;
  OleCheck(pIOpenRowset.OpenRowset(self, @dbidID, nil, IID_IUnknown, 0, nil, @fUnkInnerSQLNCLIRowset));
  SetupAccessors(self as IAccessor);
  Result := S_OK;
end;

function TIDListRowset.SetupAccessors(pIAccessorIDList: IAccessor): HRESULT;
var binding: array [0..0] of TDBBinding;
    bindStatus: array [0..0] of DWORD;
    hAccessorIDList: HACCESSOR;
begin
  FillBindingsAndSetupRowBuffer(@binding);
  bindStatus[0] := 0;
  OleCheck(pIAccessorIDList.CreateAccessor(DBACCESSOR_ROWDATA, 1, @binding, SizeOf(TIDListRec),
    hAccessorIDList, @bindStatus));
  SetAccessorHandle(0, hAccessorIDList);
  Result := S_OK;
end;


initialization
  assert(sizeof(TOleDBStatementParam) and (sizeof(Int64)-1)=0);
  TOleDBConnectionProperties.RegisterClassNameForDefinition;
  TOleDBOracleConnectionProperties.RegisterClassNameForDefinition;
  TOleDBMSOracleConnectionProperties.RegisterClassNameForDefinition;
  TOleDBMSSQLConnectionProperties.RegisterClassNameForDefinition;
  TOleDBMSSQL2005ConnectionProperties.RegisterClassNameForDefinition;
  TOleDBMSSQL2008ConnectionProperties.RegisterClassNameForDefinition;
  TOleDBMSSQL2012ConnectionProperties.RegisterClassNameForDefinition;
  TOleDBMySQLConnectionProperties.RegisterClassNameForDefinition;
  {$ifndef CPU64} // Jet is not available on Win64
  TOleDBJetConnectionProperties.RegisterClassNameForDefinition;
  {$endif}
  TOleDBACEConnectionProperties.RegisterClassNameForDefinition;
  TOleDBAS400ConnectionProperties.RegisterClassNameForDefinition;
  TOleDBODBCSQLConnectionProperties.RegisterClassNameForDefinition;

finalization
  if OleDBCoinitialized<>0 then
    SynDBLog.Add.Log(sllError,'Missing TOleDBConnection.Destroy call = %',
      OleDBCoInitialized);

{$else}

implementation

{$endif MSWINDOWS} // compiles as void unit for non-Windows - allow Lazarus package

end.
