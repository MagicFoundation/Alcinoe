unit Alcinoe.Macapi.QuartzCore;

interface

{$I Alcinoe.inc}

uses
  Macapi.ObjectiveC,
  Macapi.Foundation,
  Macapi.QuartzCore;

{$M+}

type

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported130}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-4346 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  ALCIFilterClass = interface(CIFilterClass)
    ['{4F7C579A-37D9-4547-BBE8-F1D8792B1B8F}']
  end;
  ALCIFilter = interface(CIFilter)
    ['{8E5DF888-442E-46E8-BCFB-4B6F5DF83289}']
    function outputImage: CIImage; cdecl;
  end;
  TALCIFilter = class(TOCGenericImport<ALCIFilterClass, ALCIFilter>)  end;

implementation

end.