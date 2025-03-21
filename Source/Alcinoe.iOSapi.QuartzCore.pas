unit Alcinoe.iOSapi.QuartzCore;

interface

{$I Alcinoe.inc}

uses
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.QuartzCore;

type

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2424 has been resolved. If resolved, remove the definitions below.'}
  {$ENDIF}
  CAFrameRateRange = record
    minimum: Single;
    maximum: Single;
    preferred: Single;
  end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2424 has been resolved. If resolved, remove the definitions below.'}
  {$ENDIF}
  ALCADisplayLinkClass = interface(CADisplayLinkClass)
    ['{6FB84A87-2026-4B75-83A0-DD5CFC48E812}']
  end;
  ALCADisplayLink = interface(CADisplayLink)
    ['{2CA36916-173B-43C9-B66D-FBB4EAC88FE8}']
    procedure setPreferredFramesPerSecond(preferredFramesPerSecond: NSInteger); cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2424
    function preferredFramesPerSecond : NSInteger; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2424
    procedure setPreferredFrameRateRange(preferredFrameRateRange: CAFrameRateRange); cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2424
    function preferredFrameRateRange : CAFrameRateRange; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2424
  end;
  TALCADisplayLink = class(TOCGenericImport<ALCADisplayLinkClass, ALCADisplayLink>)  end;

implementation

end.
