unit Alcinoe.iOSApi.UIKit;

interface

{$I Alcinoe.inc}

uses
  iOSapi.Foundation,
  iOSapi.CoreGraphics,
  iOSapi.UIKit,
  Macapi.ObjectiveC;

{$M+}

type

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported131}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-6237 has been resolved. If resolved, remove the functions below.'}
  {$ENDIF}
  NSValueClass = interface(NSObjectClass)
    ['{102927B7-EBE2-4511-A04A-7199E162F766}']
    {class} function valueWithCGPoint(point: CGPoint): Pointer; cdecl;
    {class} function valueWithCGRect(rect: CGRect): Pointer; cdecl;
    {class} function valueWithCGSize(size: CGSize): Pointer; cdecl;
    {class} function valueWithCGVector(vector: CGVector): Pointer; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-6237
    {class} function valueWithUIEdgeInsets(insets: UIEdgeInsets): Pointer; cdecl;
    {class} function valueWithUIOffset(insets: UIOffset): Pointer; cdecl;
  end;
  NSValue = interface(NSObject)
    ['{F758BBDA-1C05-46FC-899A-59AB9CE9F228}']
    function CGPointValue: CGPoint; cdecl;
    function CGRectValue: CGRect; cdecl;
    function CGSizeValue: CGSize; cdecl;
    function CGVectorValue: CGVector; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-6237
    function CGAffineTransformValue: CGAffineTransform; cdecl;
    function UIEdgeInsetsValue: UIEdgeInsets; cdecl;
    function UIOffsetValue: UIOffset; cdecl;
  end;
  TNSValue = class(TOCGenericImport<NSValueClass, NSValue>)  end;

implementation

end.