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
    ['{32347938-7927-4C6F-8687-47D76753B5E7}']
    {class} function valueWithCGPoint(point: CGPoint): Pointer; cdecl;
    {class} function valueWithCGRect(rect: CGRect): Pointer; cdecl;
    {class} function valueWithCGSize(size: CGSize): Pointer; cdecl;
    {class} function valueWithCGVector(vector: CGVector): Pointer; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-6237
    {class} function valueWithUIEdgeInsets(insets: UIEdgeInsets): Pointer; cdecl;
    {class} function valueWithUIOffset(insets: UIOffset): Pointer; cdecl;
  end;
  NSValue = interface(NSObject)
    ['{0539B6F1-D277-4BCA-95B2-A283B46CB248}']
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
