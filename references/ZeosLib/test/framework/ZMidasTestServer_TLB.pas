unit ZMidasTestServer_TLB;

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

// PASTLWTR : $Revision: 1.2 $
// File generated on 20.05.2004 22:45:11 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Program Files\Borland\Delphi6\Projects\Midas\Server.tlb (1)
// LIBID: {579D4B4D-90AE-464E-B5A6-71093C8FBC5A}
// LCID: 0
// Helpfile: 
// DepndLst: 
//   (1) v1.0 Midas, (C:\WINDOWS\System32\midas.dll)
//   (2) v2.0 stdole, (C:\WINDOWS\System32\stdole2.tlb)
//   (3) v4.0 StdVCL, (C:\WINDOWS\System32\STDVCL40.DLL)
// ************************************************************************ //

interface

{$I ZTestFramework.inc}

{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}

//!!!{$VARPROPSETTER ON}

//uses Windows, ActiveX, Classes, Graphics, Midas, StdVCL, Variants;
uses Midas;


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  ServerMajorVersion = 1;
  ServerMinorVersion = 0;

  LIBID_Server: TGUID = '{579D4B4D-90AE-464E-B5A6-71093C8FBC5A}';

  IID_IZRemoteDM: TGUID = '{74064AD3-A265-4560-AE26-17B334DC8D57}';
  CLASS_ZRemoteDM: TGUID = '{FD5981E3-A86F-4A2F-B20E-94D1B0799AD6}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IZRemoteDM = interface;
  IZRemoteDMDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  ZRemoteDM = IZRemoteDM;


// *********************************************************************//
// Interface: IZRemoteDM
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {74064AD3-A265-4560-AE26-17B334DC8D57}
// *********************************************************************//
  IZRemoteDM = interface(IAppServer)
    ['{74064AD3-A265-4560-AE26-17B334DC8D57}']
    procedure SetOptions(const Protocol: WideString; const HostName: WideString; Port: Integer;
                         const Database: WideString; const UserName: WideString;
                         const Password: WideString); safecall;
    procedure MasterDetail(const Value: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IZRemoteDMDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {74064AD3-A265-4560-AE26-17B334DC8D57}
// *********************************************************************//
  IZRemoteDMDisp = dispinterface
    ['{74064AD3-A265-4560-AE26-17B334DC8D57}']
    procedure SetOptions(const Protocol: WideString; const HostName: WideString; Port: Integer;
                         const Database: WideString; const UserName: WideString;
                         const Password: WideString); dispid 1;
    procedure MasterDetail(const Value: Integer); dispid 2;
    function AS_ApplyUpdates(const ProviderName: WideString; Delta: OleVariant; MaxErrors: Integer;
                             out ErrorCount: Integer; var OwnerData: OleVariant): OleVariant; dispid 20000000;
    function AS_GetRecords(const ProviderName: WideString; Count: Integer; out RecsOut: Integer; 
                           Options: Integer; const CommandText: WideString; var Params: OleVariant; 
                           var OwnerData: OleVariant): OleVariant; dispid 20000001;
    function AS_DataRequest(const ProviderName: WideString; Data: OleVariant): OleVariant; dispid 20000002;
    function AS_GetProviderNames: OleVariant; dispid 20000003;
    function AS_GetParams(const ProviderName: WideString; var OwnerData: OleVariant): OleVariant; dispid 20000004;
    function AS_RowRequest(const ProviderName: WideString; Row: OleVariant; RequestType: Integer; 
                           var OwnerData: OleVariant): OleVariant; dispid 20000005;
    procedure AS_Execute(const ProviderName: WideString; const CommandText: WideString; 
                         var Params: OleVariant; var OwnerData: OleVariant); dispid 20000006;
  end;

// *********************************************************************//
// The Class CoZRemoteDM provides a Create and CreateRemote method to          
// create instances of the default interface IZRemoteDM exposed by              
// the CoClass ZRemoteDM. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoZRemoteDM = class
    class function Create: IZRemoteDM;
    class function CreateRemote(const MachineName: string): IZRemoteDM;
  end;

implementation

uses ComObj;

class function CoZRemoteDM.Create: IZRemoteDM;
begin
  Result := CreateComObject(CLASS_ZRemoteDM) as IZRemoteDM;
end;

class function CoZRemoteDM.CreateRemote(const MachineName: string): IZRemoteDM;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ZRemoteDM) as IZRemoteDM;
end;

end.
