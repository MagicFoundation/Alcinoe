unit Alcinoe.Macapi.QuartzCore;

interface

{$I Alcinoe.inc}

uses
  Macapi.ObjectiveC,
  Macapi.CocoaTypes,
  Macapi.Foundation,
  Macapi.QuartzCore;

{$M+}

type

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1937 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  ALCIContextClass = interface(CIContextClass)
    ['{55DDC7C9-2D79-4F0D-9CDF-D62E3965503C}']
    {class} function contextWithOptions(options: NSDictionary): Pointer; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1937
  end;
  ALCIContext = interface(CIContext)
    ['{077B5A5A-4CAD-44F5-B381-22649ACD548D}']
  end;
  TALCIContext = class(TOCGenericImport<ALCIContextClass, ALCIContext>)  end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1937 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  ALCIFilterClass = interface(CIFilterClass)
    ['{4F7C579A-37D9-4547-BBE8-F1D8792B1B8F}']
  end;
  ALCIFilter = interface(CIFilter)
    ['{8E5DF888-442E-46E8-BCFB-4B6F5DF83289}']
    function outputImage: CIImage; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1937
  end;
  TALCIFilter = class(TOCGenericImport<ALCIFilterClass, ALCIFilter>)  end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1937 has been resolved. If resolved, remove the functions below.'}
{$ENDIF}
function kCIInputImageKey: NSString; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1937
function kCIInputRadiusKey: NSString; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1937
function kCIFormatRGBA8: CIFormat; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1937
function kCIFormatBGRA8: CIFormat; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1937

implementation

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1937
const
  CoreImageFwk: string = '/System/Library/Frameworks/CoreImage.framework/CoreImage';

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1937
function kCIInputImageKey: NSString;
begin
  result := CocoaNSStringConst(CoreImageFwk, 'kCIInputImageKey');
end;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1937
function kCIInputRadiusKey: NSString;
begin
  result := CocoaNSStringConst(CoreImageFwk, 'kCIInputRadiusKey');
end;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1937
function kCIFormatRGBA8: CIFormat;
begin
  Result := CocoaIntegerConst(CoreImageFwk, 'kCIFormatRGBA8');
end;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1937
function kCIFormatBGRA8: CIFormat;
begin
  Result := CocoaIntegerConst(CoreImageFwk, 'kCIFormatBGRA8');
end;

end.
