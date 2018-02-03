unit PeViewer_TLB;

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

// PASTLWTR : $Revision$
// File generated on 4.6.2000 18:23:08 from Type Library described below.

// ************************************************************************ //
// Type Lib: C:\Program Files\Borland\Delphi5\Projects\Tools\PeViewer\PeViewer.tlb (1)
// IID\LCID: {7DD35085-3A37-11D4-B06E-C61ABD372324}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\SYSTEM\StdOle2.Tlb)
// ************************************************************************ //

{$I jcl.inc}

{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 

interface

uses
  Windows, ActiveX, Classes, Graphics,
  {$IFDEF DELPHI5_UP}
  OleServer,
  {$ENDIF DELPHI5_UP}
  OleCtrls, StdVCL;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  PeViewerMajorVersion = 1;
  PeViewerMinorVersion = 0;

  LIBID_PeViewer: TGUID = '{7DD35085-3A37-11D4-B06E-C61ABD372324}';

  IID_IPeViewerControl: TGUID = '{7DD35086-3A37-11D4-B06E-C61ABD372324}';
  CLASS_PeViewerControl: TGUID = '{7DD35088-3A37-11D4-B06E-C61ABD372324}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IPeViewerControl = interface;
  IPeViewerControlDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  PeViewerControl = IPeViewerControl;


// *********************************************************************//
// Interface: IPeViewerControl
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7DD35086-3A37-11D4-B06E-C61ABD372324}
// *********************************************************************//
  IPeViewerControl = interface(IDispatch)
    ['{7DD35086-3A37-11D4-B06E-C61ABD372324}']
    procedure OpenFile(const FileName: WideString); safecall;
    procedure BringToFront; safecall;
  end;

// *********************************************************************//
// DispIntf:  IPeViewerControlDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7DD35086-3A37-11D4-B06E-C61ABD372324}
// *********************************************************************//
  IPeViewerControlDisp = dispinterface
    ['{7DD35086-3A37-11D4-B06E-C61ABD372324}']
    procedure OpenFile(const FileName: WideString); dispid 1;
    procedure BringToFront; dispid 2;
  end;

// *********************************************************************//
// The Class CoPeViewerControl provides a Create and CreateRemote method to          
// create instances of the default interface IPeViewerControl exposed by              
// the CoClass PeViewerControl. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoPeViewerControl = class
    class function Create: IPeViewerControl;
    class function CreateRemote(const MachineName: string): IPeViewerControl;
  end;

implementation

uses ComObj;

class function CoPeViewerControl.Create: IPeViewerControl;
begin
  Result := CreateComObject(CLASS_PeViewerControl) as IPeViewerControl;
end;

class function CoPeViewerControl.CreateRemote(const MachineName: string): IPeViewerControl;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_PeViewerControl) as IPeViewerControl;
end;

end.
